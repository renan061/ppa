module Main (main) where

import ParserMonad (apply)
import Scanner (Token (..), next)
import Test.HUnit (assertEqual)

main = do
  simpleTest TokenReserved "true skip false else if == while"
  simpleTest (TokenSymbol . head) "[ ) + ( ] } = { ;"
  simpleTest (TokenNum . read) "123 11 2"
  simpleTest TokenID "a potato b var x"

simpleTest :: (String -> Token) -> String -> IO ()
simpleTest f input =
  let result = run input
      expected = map f (words input)
   in assertEqual "FAIL" result expected

run :: String -> [Token]
run s = case apply next s of
  Nothing -> []
  Just (token, s) -> token : run s
