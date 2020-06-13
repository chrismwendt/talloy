#!/usr/bin/env stack
-- stack --resolver lts-15.4 --install-ghc ghci --package megaparsec --package parser-combinators --package containers --package rainbow

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Rainbow
import Data.Function

type Parser = Parsec Void String

main = do
  example <- (readFile "example.ty")
  void $ timeout 1000000 $ case parse (expression <* eof) "" example of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right parsedExpression -> do
      putChunkLn $ "Program:" & fore cyan
      putStrLn example
      putStrLn ""
      putChunkLn $ "Output:" & fore cyan
      value <- evaluate (MemorableBindings Map.empty) prelude parsedExpression
      putStrLn ""
      putChunkLn $ "Return value:" & fore cyan
      putStrLn $ "=> " ++ show value

prelude :: MemorableBindings
prelude = MemorableBindings $ Map.fromList
  [ ("print", (MemorableBindings Map.empty, EValue (PrimitiveValue "print")))
  , ("sleep", (MemorableBindings Map.empty, EValue (PrimitiveValue "sleep")))
  , ("uppercase", (MemorableBindings Map.empty, EValue (PrimitiveValue "uppercase")))
  ]

logeval :: MemorableBindings -> MemorableBindings -> Expression -> IO Value
logeval movs@(MemorableBindings ovs) mbs@(MemorableBindings bs) e = do
  -- putStrLn $ unwords (Map.keys ovs) ++ " | " ++ unwords (Map.keys bs) ++ " | " ++ pretty e
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
    (PrimitiveValue "uppercase", StringValue s) -> return $ StringValue (map toUpper s)
    (PrimitiveValue "uppercase", other) -> error $ "cannot uppercase " ++ show other
    (Lambda (MemorableBindings lbs) name expr, arg) -> logeval ovs (MemorableBindings (Map.insert name (bs, EValue arg) (lbs `Map.union` mbs))) expr
    (l, r) -> error $ show l ++ " is not a function"
evaluate ovs (MemorableBindings bs) (EValue (Lambda (MemorableBindings lbs) name expression)) = return $ Lambda (MemorableBindings (lbs `Map.union` bs)) name expression
evaluate movs@(MemorableBindings ovs) (MemorableBindings bs) (Variable v) = case ovs Map.!? v of
  Nothing -> case bs Map.!? v of
    Nothing -> error $ "unknown function " ++ v
    Just (bs', expr) -> logeval movs bs' expr
  Just (bs', expr) -> logeval movs bs' expr
evaluate ovs bs (Block []) = return Unit
evaluate ovs bs (Block [statement]) = logeval ovs bs statement
evaluate ovs bs (Block (statement : rest)) = logeval ovs bs statement >> logeval ovs bs (Block rest)
evaluate ovs bs (EValue v) = return v
-- evaluate ovs bs o = error $ "unrecognized: " ++ show o

pretty :: Expression -> String
pretty (Variable v) = "$" ++ v
pretty (FunctionCall f a) = "(" ++ pretty f ++ " " ++ pretty a ++ ")"
pretty (Block statements) = "{ " ++ concatMap (\s -> pretty s ++ "; ") statements ++ "}"
pretty (Let bindings expr) = "(let { " ++ concatMap (\(n, e) -> n ++ " = " ++ pretty e ++ "; ") (Map.toList bindings) ++ " } in (" ++ pretty expr ++ "))"
pretty (Override bindings expr) = "(override { " ++ concatMap (\(n, (old, e)) -> n ++ ":" ++ old ++ " = " ++ pretty e ++ "; ") (Map.toList bindings) ++ " } in (" ++ pretty expr ++ "))"
pretty (EValue v) = prettyV v

prettyV :: Value -> String
prettyV (PrimitiveValue n) = "name#"
prettyV (StringValue v) = "\"" ++ v ++ "\""
prettyV (NumberValue v) = "#" ++ show v
prettyV (Lambda _ n e) = "(\\" ++ n ++ " -> " ++ pretty e ++ ")"
prettyV Unit = "Unit"

data Expression =
    Variable String
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
  | Lambda MemorableBindings String Expression
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
    return $ EValue $ Lambda (MemorableBindings Map.empty) name e
  , EValue . StringValue <$> (char '"' *> manyTill anySingle (char '"') <* whitespace)
  , EValue . NumberValue <$> L.float
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
