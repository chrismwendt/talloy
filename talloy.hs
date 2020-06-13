#!/usr/bin/env stack
-- stack --resolver lts-15.4 --install-ghc ghci --package megaparsec --package parser-combinators --package containers

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

type Parser = Parsec Void String

main = do
  example <- (readFile "example.ty")
  void $ timeout 1000000 $ case parse (expression <* eof) "" example of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right parsedExpression -> do
      value <- evaluate Map.empty parsedExpression
      putStrLn $ "=> " ++ show value

evaluate :: Bindings -> Expression -> IO Value
evaluate bs (Let bindings expression) = evaluate (bindings `Map.union` bs) expression
evaluate bs (FunctionCall function argument) = do
  argument' <- evaluate bs argument
  function' <- evaluate bs function
  case (function', argument') of
    (PrimitiveValue "print", StringValue s) -> Unit <$ putStrLn s
    (PrimitiveValue "print", other) -> Unit <$ print other
    (PrimitiveValue "sleep", NumberValue n) -> Unit <$ threadDelay (floor (n * 10**6))
    (PrimitiveValue "sleep", other) -> error "sleep expects a number"
    (l, r) -> error $ show l ++ " is not a function"
evaluate bs (Variable "print") = return (PrimitiveValue "print")
evaluate bs (Variable "sleep") = return (PrimitiveValue "sleep")
evaluate bs (Variable v) = evaluate bs $ bs Map.! v
evaluate bs (StringLiteral str) = return (StringValue str)
evaluate bs (NumberLiteral number) = return (NumberValue number)
evaluate bs (Block []) = return Unit
evaluate bs (Block [statement]) = evaluate bs statement
evaluate bs (Block (statement : rest)) = evaluate bs statement >> evaluate bs (Block rest)
-- evaluate bs o = error $ "unrecognized: " ++ show o

data Expression =
    Variable String
  | StringLiteral String
  | NumberLiteral Float
  | FunctionCall Expression Expression
  | Block [Expression]
  -- Let newBindings expr
  | Let Bindings Expression
  deriving (Show)

data Value =
    PrimitiveValue String
  | StringValue String
  | NumberValue Float
  | Unit
  deriving (Show)

type Bindings = Map String Expression

term :: Parser Expression
term = choice
  [ letExpr
  , StringLiteral <$> (char '"' *> manyTill anySingle (char '"') <* whitespace)
  , NumberLiteral <$> L.float
  , Variable <$> some alphaNumChar <* whitespace
  , Block <$> (charTok '{' *> sepEndBy1 expression (charTok ';') <* charTok '}')
  ]

letExpr :: Parser Expression
letExpr = do
  strTok "let"
  let binding = (,) <$> some alphaNumChar <* whitespace <* charTok '=' <*> expression
  m <- charTok '{' *> sepEndBy1 binding (charTok ';') <* charTok '}'
  strTok "in"
  e <- term
  return $ Let (Map.fromList m) e

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
