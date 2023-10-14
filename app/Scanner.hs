module Scanner
  ( Token (..),
    next,
    tokenID,
    tokenNum,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isDigit, isLetter, isSpace)
import Data.Functor (void, ($>))
import Data.List (uncons)
import Data.Maybe (isJust)
import ParserMonad (Parser (Parser), apply, choice, failure, many0, many1)

-------------------------------------------------------------------------------

data Token
  = TokenNum Int
  | TokenReserved String
  | TokenID String
  | TokenSymbol Char

instance Eq Token where
  (TokenNum n1) == (TokenNum n2) = n1 == n2
  (TokenReserved s1) == (TokenReserved s2) = s1 == s2
  (TokenID s1) == (TokenID s2) = s1 == s2
  (TokenSymbol c1) == (TokenSymbol c2) = c1 == c2
  _ == _ = False

instance Show Token where
  show (TokenNum n) = show n
  show (TokenReserved s) = "R:" ++ s
  show (TokenID s) = "I:" ++ s
  show (TokenSymbol c) = show c

tokenNum :: Token -> Maybe Int
tokenNum (TokenNum n) = Just n
tokenNum _ = Nothing

tokenID :: Token -> Maybe String
tokenID (TokenID x) = Just x
tokenID _ = Nothing

-------------------------------------------------------------------------------

next :: Parser Token
next = token <* spaces

-------------------------------------------------------------------------------

input :: Parser Char
input = Parser uncons

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do c <- input; if f c then return c else failure

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string [] = return []
string s@(c : cs) = (char c *> string cs) $> s

spaces :: Parser ()
spaces = void $ many0 (satisfy isSpace)

-------------------------------------------------------------------------------

number :: Parser Token
number = TokenNum . read <$> many1 (satisfy isDigit)

reserved :: Parser Token
reserved =
  let f s = TokenReserved <$> string s
   in (choice . map f) ["true", "false", "skip", "if", "else", "while", "=="]

identifier :: Parser Token
identifier = TokenID <$> many1 (satisfy isLetter)

symbol :: Parser Token
symbol =
  let f c = TokenSymbol <$> char c
   in (choice . map f) ['(', ')', '[', ']', '{', '}', '=', ';', '+']

token :: Parser Token
token = number <|> reserved <|> identifier <|> symbol
