#!/usr/bin/env stack
-- stack --resolver lts-15.4 --install-ghc ghci --package megaparsec --package parser-combinators

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad
import System.Timeout
import Control.Concurrent

type Parser = Parsec Void String

main = do
  example <- (readFile "example.ty")
  void $ timeout 1000000 $ case parse (expression <* eof) "" example of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right parsedExpression -> do
      value <- evaluate parsedExpression
      putStrLn $ "=> " ++ show value

evaluate :: Expression -> IO Value
evaluate (FunctionCall function argument) = do
  function' <- evaluate function
  argument' <- evaluate argument
  case (function', argument') of
    (PrimitiveValue "print", StringValue s) -> Unit <$ putStrLn s
    (PrimitiveValue "print", other) -> Unit <$ print other
    (PrimitiveValue "sleep", NumberValue n) -> Unit <$ threadDelay (floor (n * 10**6))
    (PrimitiveValue "sleep", other) -> error "sleep expects a number"
    (l, r) -> error $ show l ++ " is not a function"
evaluate (Variable "print") = return (PrimitiveValue "print")
evaluate (Variable "sleep") = return (PrimitiveValue "sleep")
evaluate (StringLiteral str) = return (StringValue str)
evaluate (NumberLiteral number) = return (NumberValue number)
evaluate (Block []) = return Unit
evaluate (Block [statement]) = evaluate statement
evaluate (Block (statement : rest)) = evaluate statement >> evaluate (Block rest)
evaluate o = error $ "unrecognized: " ++ show o

data Expression =
    Variable String
  | StringLiteral String
  | NumberLiteral Float
  | FunctionCall Expression Expression
  | Block [Expression]
  deriving (Show)

data Value =
    PrimitiveValue String
  | StringValue String
  | NumberValue Float
  | Unit
  deriving (Show)

term :: Parser Expression
term = choice
  [ StringLiteral <$> (char '"' *> manyTill anySingle (char '"') <* whitespace)
  , NumberLiteral <$> L.float
  , Variable <$> some letterChar <* whitespace
  , Block <$> (char '{' *> whitespace *> sepEndBy1 expression (char ';' <* whitespace) <* char '}' <* whitespace)
  ]

expression :: Parser Expression
expression = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ binary (void $ many (char ' ')) FunctionCall
    ]
  ]

binary :: Parser () -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary name f = InfixL  (f <$ name)

whitespace :: Parser ()
whitespace = space
