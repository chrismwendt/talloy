#!/usr/bin/env stack
-- stack --resolver lts-15.4 --install-ghc ghci --package megaparsec --package parser-combinators --package containers

{-# LANGUAGE TupleSections #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad
import System.Timeout
import Control.Concurrent
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.Char

type Parser = Parsec Void String

main = do
  example <- (readFile "example.ty")
  void $ timeout 1000000 $ case parse (expression <* eof) "" example of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right parsedExpression -> do
      value <- evaluate (MemorableBindings Map.empty) (MemorableBindings Map.empty) parsedExpression
      putStrLn $ "=> " ++ show value

logeval :: MemorableBindings -> MemorableBindings -> Expression -> IO Value
logeval movs@(MemorableBindings ovs) mbs@(MemorableBindings bs) e = do
  putStrLn $ unwords (Map.keys ovs) ++ " | " ++ unwords (Map.keys bs) ++ " | " ++ pretty e
  evaluate movs mbs e

evaluate :: MemorableBindings -> MemorableBindings -> Expression -> IO Value
evaluate ovs (MemorableBindings bs) (Let bindings expression) =
  let bs' = MemorableBindings $ bs `Map.union` (Map.fromList $ map (\(name, expr) -> (name, (MemorableBindings bs, expr))) (Map.toList bindings))
  in logeval ovs bs' expression
evaluate (MemorableBindings ovs) bs@(MemorableBindings mbs) (Override bindings expression) =
  let ovs' = MemorableBindings $ ovs `Map.union` (Map.fromList $ map (\(name, (old, expr)) -> (name, (MemorableBindings (Map.insert old (mbs Map.! name) mbs), expr))) (Map.toList bindings))
  in logeval ovs' bs expression
evaluate ovs bs@(MemorableBindings mbs) (FunctionCall function argument) = do
  argument' <- logeval ovs bs argument
  function' <- logeval ovs bs function
  case (function', argument') of
    (PrimitiveValue "print", StringValue s) -> Unit <$ putStrLn s
    (PrimitiveValue "print", other) -> Unit <$ print other
    (PrimitiveValue "sleep", NumberValue n) -> Unit <$ threadDelay (floor (n * 10**6))
    (PrimitiveValue "sleep", other) -> error "sleep expects a number"
    (PrimitiveValue "TOUPPER", StringValue s) -> return $ StringValue (map toUpper s)
    (PrimitiveValue "TOUPPER", other) -> error $ "cannot TOUPPER " ++ show other
    (Lambda name expr, arg) -> logeval ovs (MemorableBindings (Map.insert name (bs, EValue arg) mbs)) expr
    (l, r) -> error $ show l ++ " is not a function"
evaluate ovs bs (EValue (Lambda name expression)) = return $ Lambda name expression
evaluate movs@(MemorableBindings ovs) (MemorableBindings bs) (Variable v) = case ovs Map.!? v of
  Nothing -> case bs Map.!? v of
    Nothing -> case v of
      "print" -> return (PrimitiveValue "print")
      "sleep" -> return (PrimitiveValue "sleep")
      "TOUPPER" -> return (PrimitiveValue "TOUPPER")
      other -> error $ "unknown function " ++ other
    Just (bs', expr) -> logeval movs bs' expr
  Just (bs', expr) -> logeval movs bs' expr
evaluate ovs bs (StringLiteral str) = return (StringValue str)
evaluate ovs bs (NumberLiteral number) = return (NumberValue number)
evaluate ovs bs (Block []) = return Unit
evaluate ovs bs (Block [statement]) = logeval ovs bs statement
evaluate ovs bs (Block (statement : rest)) = logeval ovs bs statement >> logeval ovs bs (Block rest)
evaluate ovs bs (EValue v) = return v
-- evaluate ovs bs o = error $ "unrecognized: " ++ show o

pretty :: Expression -> String
pretty (Variable v) = "$" ++ v
pretty (StringLiteral v) = "!" ++ v
pretty (NumberLiteral v) = "#" ++ show v
pretty (FunctionCall f a) = "(" ++ pretty f ++ " " ++ pretty a ++ ")"
pretty (Block statements) = "{ " ++ concatMap (\s -> pretty s ++ "; ") statements ++ "}"
pretty (Let bindings expr) = "(let { " ++ concatMap (\(n, e) -> n ++ " = " ++ pretty e ++ "; ") (Map.toList bindings) ++ " } in (" ++ pretty expr ++ "))"
pretty (Override bindings expr) = "(override { " ++ concatMap (\(n, (old, e)) -> n ++ ":" ++ old ++ " = " ++ pretty e ++ "; ") (Map.toList bindings) ++ " } in (" ++ pretty expr ++ "))"
pretty (EValue v) = prettyV v

prettyV :: Value -> String
prettyV (PrimitiveValue n) = "name#"
prettyV (StringValue v) = "\"" ++ v ++ "\""
prettyV (NumberValue v) = "#" ++ show v
prettyV (Lambda n e) = "(\\" ++ n ++ " -> " ++ pretty e ++ ")"
prettyV Unit = "Unit"

data Expression =
    Variable String
  | StringLiteral String
  | NumberLiteral Float
  | FunctionCall Expression Expression
  | Block [Expression]
  | Let PlainBindings Expression
  | Override (Map String (String, Expression)) Expression
  | EValue Value
  deriving (Show)

data Value =
    PrimitiveValue String
  | StringValue String
  | NumberValue Float
  | Lambda String Expression
  | Unit
  deriving (Show)

type PlainBindings = Map String Expression

data MemorableBindings = MemorableBindings (Map String (MemorableBindings, Expression)) deriving (Show)

term :: Parser Expression
term = choice
  [ do
    (bindings, expr) <- bindingExpr "let" (some alphaNumChar)
    return $ Let (Map.fromList bindings) expr
  , do
    let p = do
          name <- some alphaNumChar
          char ':'
          oldname <- some alphaNumChar
          return (name, oldname)
    (bindings, expr) <- bindingExpr "override" p
    return $ Override (Map.fromList $ map (\((name, old), e) -> (name, (old, e))) bindings) expr
  , charTok '(' *> expression <* charTok ')'
  , do
    charTok '\\'
    name <- some alphaNumChar <* whitespace
    strTok "->"
    e <- expression
    return $ EValue $ Lambda name e
  , StringLiteral <$> (char '"' *> manyTill anySingle (char '"') <* whitespace)
  , NumberLiteral <$> L.float
  , Variable <$> some alphaNumChar <* whitespace
  , Block <$> (charTok '{' *> sepEndBy1 expression (charTok ';') <* charTok '}')
  ]

bindingExpr :: String -> Parser a -> Parser ([(a, Expression)], Expression)
bindingExpr keyword aP = do
  strTok keyword
  let binding = (,) <$> aP <* whitespace <* charTok '=' <*> expression
  m <- charTok '{' *> sepEndBy1 binding (charTok ';') <* charTok '}'
  strTok "in"
  e <- expression
  return $ (m, e)

expression :: Parser Expression
expression = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ binary (void $ many (char ' ')) FunctionCall
    ]
  ]

binary :: Parser () -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary name f = InfixL (f <$ name)

whitespace :: Parser ()
whitespace = space

token :: Parser a -> Parser a
token p = p <* whitespace

charTok :: Char -> Parser Char
charTok c = char c <* whitespace

strTok :: String -> Parser String
strTok str = string str <* whitespace
