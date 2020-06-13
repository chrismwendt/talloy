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

type Parser = Parsec Void String

main = do
  example <- (readFile "example.ty")
  void $ timeout 1000000 $ case parse (expression <* eof) "" example of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right parsedExpression -> do
      value <- evaluate (MemorableBindings Map.empty) (MemorableBindings Map.empty) parsedExpression
      putStrLn $ "=> " ++ show value

evaluate :: MemorableBindings -> MemorableBindings -> Expression -> IO Value
evaluate ovs (MemorableBindings bs) (Let bindings expression) =
  let bs' = MemorableBindings $ bs `Map.union` (Map.fromList $ map (\(name, expr) -> (name, (MemorableBindings bs, expr))) (Map.toList bindings))
  in evaluate ovs bs' expression
evaluate (MemorableBindings ovs) bs (Override bindings expression) =
  let ovs' = MemorableBindings $ ovs `Map.union` (Map.fromList $ map (\(name, expr) -> (name, (MemorableBindings ovs, expr))) (Map.toList bindings))
  in evaluate ovs' bs expression
evaluate ovs bs (FunctionCall function argument) = do
  argument' <- evaluate ovs bs argument
  function' <- evaluate ovs bs function
  case (function', argument') of
    (PrimitiveValue "print", StringValue s) -> Unit <$ putStrLn s
    (PrimitiveValue "print", other) -> Unit <$ print other
    (PrimitiveValue "sleep", NumberValue n) -> Unit <$ threadDelay (floor (n * 10**6))
    (PrimitiveValue "sleep", other) -> error "sleep expects a number"
    (l, r) -> error $ show l ++ " is not a function"
evaluate ovs bs (Variable "print") = return (PrimitiveValue "print")
evaluate ovs bs (Variable "sleep") = return (PrimitiveValue "sleep")
evaluate movs@(MemorableBindings ovs) (MemorableBindings bs) (Variable v) = traceShow (Map.keys ovs) $ case ovs Map.!? v of
  Nothing -> case bs Map.!? v of
    Nothing -> error "BLAH"
    Just (bs', expr) -> evaluate movs bs' expr
  Just (bs', expr) -> evaluate movs bs' expr
evaluate ovs bs (StringLiteral str) = return (StringValue str)
evaluate ovs bs (NumberLiteral number) = return (NumberValue number)
evaluate ovs bs (Block []) = return Unit
evaluate ovs bs (Block [statement]) = evaluate ovs bs statement
evaluate ovs bs (Block (statement : rest)) = evaluate ovs bs statement >> evaluate ovs bs (Block rest)
-- evaluate ovs bs o = error $ "unrecognized: " ++ show o

data Expression =
    Variable String
  | StringLiteral String
  | NumberLiteral Float
  | FunctionCall Expression Expression
  | Block [Expression]
  | Let PlainBindings Expression
  | Override PlainBindings Expression
  deriving (Show)

data Value =
    PrimitiveValue String
  | StringValue String
  | NumberValue Float
  | Unit
  deriving (Show)

type PlainBindings = Map String Expression

data MemorableBindings = MemorableBindings (Map String (MemorableBindings, Expression)) deriving (Show)

term :: Parser Expression
term = choice
  [ do
    (bindings, expr) <- bindingExpr "let"
    return $ Let bindings expr
  , do
    (bindings, expr) <- bindingExpr "override"
    return $ Override bindings expr
  , StringLiteral <$> (char '"' *> manyTill anySingle (char '"') <* whitespace)
  , NumberLiteral <$> L.float
  , Variable <$> some alphaNumChar <* whitespace
  , Block <$> (charTok '{' *> sepEndBy1 expression (charTok ';') <* charTok '}')
  ]

bindingExpr :: String -> Parser (PlainBindings, Expression)
bindingExpr keyword = do
  strTok keyword
  let binding = (,) <$> some alphaNumChar <* whitespace <* charTok '=' <*> expression
  m <- charTok '{' *> sepEndBy1 binding (charTok ';') <* charTok '}'
  strTok "in"
  e <- term
  return $ ((Map.fromList m), e)

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
