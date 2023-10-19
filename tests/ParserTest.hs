module Main (main) where

import AST (S)
import Parser (parse)
import Test.HUnit (assertBool, assertFailure)

--  FAILING: while [x == 1]3 {};"

main = do
  run "[a = 2]1;"
  run "[skip]2;"
  run "if [true]1 {} else {};"
  run "while [false]2 {};"
  run "[skip]2; [a = 2]3;"
  putStrLn "Finished"

run :: String -> IO ()
run s = either fail succeed (parse s)
  where
    fail = assertFailure . (++) "unparsed: "
    succeed _ = assertBool "" True
