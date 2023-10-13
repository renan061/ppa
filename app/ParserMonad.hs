module ParserMonad where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (ap, liftM, (>=>))

newtype Parser a = Parser (String -> Maybe (a, String))

apply :: Parser a -> String -> Maybe (a, String)
apply (Parser p) = p

failure :: Parser a
failure = Parser (const Nothing)

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Alternative Parser where
  empty = failure
  Parser f <|> Parser g = Parser $ \s -> f s <|> g s

instance Monad Parser where
  p >>= f = Parser $ apply p >=> (\(a, s) -> apply (f a) s)

many0 :: Parser a -> Parser [a]
many0 p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do x <- p; xs <- many0 p; return (x : xs)

choice :: [Parser a] -> Parser a
choice [] = failure
choice [p] = p
choice (p : ps) = p <|> choice ps
