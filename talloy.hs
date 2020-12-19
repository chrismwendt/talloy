#!/usr/bin/env stack
-- stack --resolver lts-16.26 --install-ghc ghci --package megaparsec --package parser-combinators --package containers --package rainbow

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad
import System.Timeout
import Control.Concurrent
import Data.Map (Map)
import Data.List (intercalate)
import qualified Data.Map as Map
import Debug.Trace
import Data.Char
import Rainbow
import Data.Function
import Data.IORef
import Control.Exception (throwIO, ArithException(Overflow))

type Parser = Parsec Void String

main2 = print $ wc "it began with two words"

wc :: String -> Int
wc s =
  let
    loop inword str = if null str
      then 0
      else if head str == ' '
        then loop False (tail str)
        else (if inword then 0 else 1) + loop True (tail str)
  in
    loop False s

main = do
  example <- fmap stripComments (readFile "example2.ty")
  void $ timeout 1000000 $ case parse (expression <* eof) "" example of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right parsedExpression -> do
      revlinesref <- newIORef []
      putStrLn $ pretty parsedExpression
      value <- evaluate (\line -> atomicModifyIORef' revlinesref (\x -> (line : x, ()))) (MemorableBindings Map.empty) prelude parsedExpression
      revlines <- readIORef revlinesref
      writeFile "output.hs" $ unlines
        [ "Program:"
        , example
        , "Output:"
        , unlines $ reverse revlines
        , "Return value:"
        , "=> " ++ prettyV value
        ]

prelude :: MemorableBindings
prelude = MemorableBindings $ Map.fromList
  [ ("print", (MemorableBindings Map.empty, EValue (PrimitiveValue "print")))
  , ("sleep", (MemorableBindings Map.empty, EValue (PrimitiveValue "sleep")))
  , ("uppercase", (MemorableBindings Map.empty, EValue (PrimitiveValue "uppercase")))
  , ("readFile", (MemorableBindings Map.empty, EValue (PrimitiveValue "readFile")))
  , ("tail", (MemorableBindings Map.empty, EValue (PrimitiveValue "tail")))
  , ("null", (MemorableBindings Map.empty, EValue (PrimitiveValue "null")))
  , ("head", (MemorableBindings Map.empty, EValue (PrimitiveValue "head")))
  , ("==", (MemorableBindings Map.empty, EValue (PrimitiveValue "==")))
  , ("+", (MemorableBindings Map.empty, EValue (PrimitiveValue "+")))
  ]

logeval :: (String -> IO ()) -> MemorableBindings -> MemorableBindings -> Expression -> IO Value
logeval out movs@(MemorableBindings ovs) mbs@(MemorableBindings bs) e = do
  putStrLn $ unwords (Map.keys ovs) ++ " | " ++ unwords (Map.keys bs) ++ " | " ++ pretty e
  evaluate out movs mbs e

evaluate :: (String -> IO ()) -> MemorableBindings -> MemorableBindings -> Expression -> IO Value
evaluate out ovs (MemorableBindings bs) (Let bindings expression) =
  let bs' = MemorableBindings $ bs `Map.union` (Map.fromList $ map (\(name, expr) -> (name, (MemorableBindings bs, expr))) (Map.toList bindings))
  in logeval out ovs bs' expression
evaluate out (MemorableBindings ovs) bs@(MemorableBindings mbs) (Override bindings expression) =
  let ovs' = MemorableBindings $ ovs `Map.union` (Map.fromList $ map (\(name, (old, expr)) -> (name, (MemorableBindings (Map.insert old (mbs Map.! name) mbs), expr))) (Map.toList bindings))
  in logeval out ovs' bs expression
evaluate out ovs bs@(MemorableBindings mbs) (FunctionCall function argument) = do
  argument' <- logeval out ovs bs argument
  function' <- logeval out ovs bs function
  case (function', argument') of
    (PrimitiveValue "print", StringValue s) -> Unit <$ out s
    (PrimitiveValue "print", other) -> Unit <$ out (prettyV other)
    (PrimitiveValue "sleep", NumberValue n) -> Unit <$ threadDelay (floor (n * 10**6))
    (PrimitiveValue "sleep", other) -> error "sleep expects a number"
    (PrimitiveValue "uppercase", StringValue s) -> return $ StringValue (map toUpper s)
    (PrimitiveValue "uppercase", other) -> error $ "cannot uppercase " ++ prettyV other
    (PrimitiveValue "readFile", StringValue filename) -> StringValue <$> readFile filename
    (PrimitiveValue "head", StringValue str) -> return $ StringValue [head str]
    (PrimitiveValue "tail", StringValue str) -> return $ StringValue (tail str)
    (PrimitiveValue "null", StringValue str) -> return $ BooleanValue (null str)
    (PrimFnValue f, arg) -> return $ f arg
    (PrimitiveValue "==", v1) -> return $ PrimFnValue (BooleanValue . (v1 ==))
    (PrimitiveValue "+", NumberValue num) -> return $ PrimFnValue (\case
      NumberValue v -> NumberValue (num + v)
      other -> error $ "can't add " ++ prettyV other)
    (Lambda (MemorableBindings lbs) name expr, arg) -> logeval out ovs (MemorableBindings (Map.insert name (bs, EValue arg) (lbs `Map.union` mbs))) expr
    (l, r) -> error $ prettyV l ++ " is not a function"
evaluate out ovs (MemorableBindings bs) (EValue (Lambda (MemorableBindings lbs) name expression)) = return $ Lambda (MemorableBindings (lbs `Map.union` bs)) name expression
evaluate out movs@(MemorableBindings ovs) (MemorableBindings bs) (Variable v) = case ovs Map.!? v of
  Nothing -> case bs Map.!? v of
    Nothing -> error $ "unknown name " ++ v
    Just (bs', expr) -> logeval out movs bs' expr
  Just (bs', expr) -> logeval out movs bs' expr
evaluate out ovs bs (Block []) = return Unit
evaluate out ovs bs (Block [statement]) = logeval out ovs bs statement
evaluate out ovs bs (Block (statement : rest)) = logeval out ovs bs statement >> logeval out ovs bs (Block rest)
evaluate out ovs bs (EValue v) = return v
evaluate out ovs bs (If b e1 e2) = do
  b' <- logeval out ovs bs b
  case b' of
    (BooleanValue b) -> if b then logeval out ovs bs e1 else logeval out ovs bs e2
    x -> error "The input to the if expression was not a boolean"
-- evaluate ovs bs o = error $ "unrecognized: " ++ show o

pretty :: Expression -> String
pretty (Variable v) = "$" ++ v
pretty (FunctionCall f a) = "(" ++ pretty f ++ " " ++ pretty a ++ ")"
pretty (Block statements) = "{ " ++ concatMap (\s -> pretty s ++ "; ") statements ++ "}"
pretty (Let bindings expr) = "(let { " ++ concatMap (\(n, e) -> n ++ " = " ++ pretty e ++ "; ") (Map.toList bindings) ++ " } in (" ++ pretty expr ++ "))"
pretty (Override bindings expr) = "(override { " ++ concatMap (\(n, (old, e)) -> n ++ ":" ++ old ++ " = " ++ pretty e ++ "; ") (Map.toList bindings) ++ " } in (" ++ pretty expr ++ "))"
pretty (If b e1 e2) = "(if (" ++ (pretty b) ++ ") then (" ++ pretty e1 ++ ") else (" ++ pretty e2 ++ "))"
pretty (EValue v) = prettyV v

prettyV :: Value -> String
prettyV (PrimitiveValue n) = n ++ "#"
prettyV (PrimFnValue n) = "primFn"
prettyV (StringValue v) = "\"" ++ v ++ "\""
prettyV (NumberValue v) = "#" ++ show v
prettyV (BooleanValue v) = "#" ++ show v
prettyV (Lambda _ n e) = "(\\" ++ n ++ " -> " ++ pretty e ++ ")"
prettyV Unit = "Unit"

data Expression =
    Variable String
  | FunctionCall Expression Expression
  | Block [Expression]
  | Let PlainBindings Expression
  | Override (Map String (String, Expression)) Expression
  | If Expression Expression Expression
  | EValue Value

data Value =
    PrimitiveValue String
  | StringValue String
  | NumberValue Float
  | BooleanValue Bool
  | PrimFnValue (Value -> Value)
  | Lambda MemorableBindings String Expression
  | Unit

instance Eq Value where
  PrimitiveValue l == PrimitiveValue r = l == r
  StringValue l == StringValue r = l == r
  NumberValue l == NumberValue r = l == r
  BooleanValue l == BooleanValue r = l == r
  PrimFnValue l == PrimFnValue r = False -- TODO can represent fns as expressions and check for equality?
  Lambda lbs ln le == Lambda rbs rn re = False -- can check for equality?
  Unit == Unit = True
  _ == _ = False

type PlainBindings = Map String Expression

newtype MemorableBindings = MemorableBindings (Map String (MemorableBindings, Expression))

term :: Parser Expression
term = choice
  [
    -- Let Bindings
    do
      (bindings, expr) <- bindingExpr "let" (some alphaNumChar)
      return $ Let (Map.fromList bindings) expr

    -- Overrides
  , do
      let p = do
            name <- some alphaNumChar
            char ':'
            oldname <- some alphaNumChar
            return (name, oldname)
      (bindings, expr) <- bindingExpr "override" p
      return $ Override (Map.fromList $ map (\((name, old), e) -> (name, (old, e))) bindings) expr

    -- Nested Expressions
  , charTok '(' *> expression <* charTok ')'

    -- If Statements
  , ifExpr

    -- Lambdas
  , do
      charTok '\\'
      name <- some alphaNumChar <* whitespace
      strTok "->"
      e <- expression
      return $ EValue $ Lambda (MemorableBindings Map.empty) name e

    -- String Values
  , EValue . StringValue <$> (char '"' *> manyTill anySingle (char '"') <* whitespace)

    -- Number Values
  , EValue . NumberValue <$> L.decimal <* whitespace

    -- Boolean Values
  , EValue . BooleanValue <$> fmap read (strTok "True" <|> strTok "False")

    -- Variable definitions
  , Variable <$> some alphaNumChar <* whitespace

    -- Blocks
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

ifExpr :: Parser Expression
ifExpr = do
  strTok "if"
  b <- term
  strTok "then"
  e1 <- term
  strTok "else"
  e2 <- term
  return $ If b e1 e2

expression :: Parser Expression
expression = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ binary (void $ many (char ' ') *> notFollowedBy (char '+')) FunctionCall ]
  , [ InfixL (do
      charTok '+'
      return (\l r -> FunctionCall (FunctionCall (Variable "+") l) r))
    ]
  , [ InfixL (do
      strTok "=="
      return (\l r -> FunctionCall (FunctionCall (Variable "==") l) r))
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

-- Mostly taken from
-- https://stackoverflow.com/questions/12940490/how-would-i-strip-out-comments-from-a-file-with-parsec
stripComments s = fromRight $ parse eatComments "" s
  where
    fromRight x = case x of
      Left _ -> error "Could not strip comments"
      Right r -> r

    comment :: Parser ()
    comment =
        (string "--" >> manyTill anySingle newline >> whitespace >> return ()) <|>
        (string "{-" >> manyTill anySingle (string "-}") >>  whitespace >> return ())

    notComment = manyTill anySingle (lookAhead (comment <|> eof))

    eatComments :: Parser String
    eatComments = do
      optional comment
      xs <- sepBy notComment comment
      optional comment
      return $ intercalate "" xs
