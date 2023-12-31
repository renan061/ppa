{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Parser (parse, parseOrFail) where

import AST (A (..), B (..), Label, S (..))
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Data.Functor (($>))
import ParserMonad (Parser, apply, failure, many0)
import Scanner (Token (..), next, tokenID, tokenNum)
import Prelude hiding (exp, seq)

-------------------------------------------------------------------------------

parse :: String -> Either String S
parse s = case apply stmts s of
  Nothing -> Left "empty"
  Just (stmt, "") -> Right stmt
  Just (stmt, s) -> Left s

parseOrFail :: String -> S
parseOrFail s = case parse s of
  Left err -> error ("failed to parse: " ++ err)
  Right stmt -> stmt

-------------------------------------------------------------------------------

satisfy :: (Token -> Bool) -> Parser ()
satisfy f = do token <- next; (guard . f) token

expect :: Token -> Parser ()
expect token = satisfy (== token)

get :: (Token -> Maybe a) -> Parser a
get f = next >>= maybe failure return . f

label :: Parser Label
label = get tokenNum

paren :: Parser a -> Parser a
paren p = do
  expect (TokenSymbol '(')
  x <- p
  expect (TokenSymbol ')')
  return x

-- to deal with ambiguities in the grammar
-- unused for now
amb :: Parser a -> [Parser a] -> Parser a
amb = foldr (<|>)

-------------------------------------------------------------------------------

num :: Parser A
num = Num <$> get tokenNum

idA :: Parser A
idA = AId <$> get tokenID

literalA :: Parser A
literalA = num <|> idA <|> paren exp

binop :: Parser A
binop = p <|> literalA
  where
    p = do
      a1 <- literalA
      expect (TokenSymbol '+')
      a2 <- binop
      return (Add a1 a2)

exp :: Parser A
exp = binop

-------------------------------------------------------------------------------

idB :: Parser B
idB = BId <$> get tokenID

tt :: Parser B
tt = expect (TokenReserved "true") $> BTrue

ff :: Parser B
ff = expect (TokenReserved "false") $> BFalse

literalB :: Parser B
literalB = tt <|> ff <|> idB <|> paren boolean

eq :: Parser B
eq = p <|> literalB
  where
    p = do
      a1 <- literalB
      expect (TokenReserved "==")
      a2 <- eq
      return $ Eq a1 a2

boolean :: Parser B
boolean = eq

-------------------------------------------------------------------------------

skip :: Parser S
skip = do
  expect (TokenSymbol '[')
  expect (TokenReserved "skip")
  expect (TokenSymbol ']')
  l <- label
  return $ Skip l

asg :: Parser S
asg = do
  expect (TokenSymbol '[')
  x <- get tokenID
  expect (TokenSymbol '=')
  a <- exp
  expect (TokenSymbol ']')
  l <- label
  return $ Asg x a l

ifelse :: Parser S
ifelse = do
  expect (TokenReserved "if")
  (b, l) <- cond
  s1 <- block
  expect (TokenReserved "else")
  s2 <- block
  return $ If l b s1 s2

while :: Parser S
while = do
  expect (TokenReserved "while")
  (b, l) <- cond
  s <- block
  return $ While l b s

seq :: Parser S
seq = do
  s1 <- stmt
  expect (TokenSymbol ';')
  s2 <- stmts
  return $ Seq s1 s2

stmt :: Parser S
stmt = skip <|> asg <|> ifelse <|> while

stmts :: Parser S
stmts = seq <|> stmt

-------------------------------------------------------------------------------

cond :: Parser (B, Label)
cond = do
  expect (TokenSymbol '[')
  b <- boolean
  expect (TokenSymbol ']')
  l <- label
  return (b, l)

block :: Parser S
block = do
  expect (TokenSymbol '{')
  s <- stmts
  expect (TokenSymbol '}')
  return s
