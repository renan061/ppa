module Main (main) where

import AST (A (..), B (..), Block (..), S)
import Data.List (sort)
import Data.Matrix (fromLists, prettyMatrix)
import DataFlow
  ( bflow,
    blocks,
    fflow,
    final,
    initial,
    labels,
    reachingDefinitions,
  )
import Parser (parseOrFail)
import Test.HUnit (Test, assertFailure, runTestTTAndExit, (~=?))

stmt =
  parseOrFail
    "[x = 5]1; [y = 1]2; while [false]3 { [y = x + y]4; [x = x + 1]5 }"

main =
  do
    print stmt
    -- runTestTT (testInitial stmt)
    -- runTestTT (testFinal stmt)
    -- runTestTTAndExit (testBlocks stmt)
    -- runTestTTAndExit (testLabels stmt)
    -- runTestTTAndExit (testFowardFlow stmt)
    -- runTestTTAndExit (testBackwardFlow stmt)
    runTestTTAndExit (testRD stmt)

testInitial :: S -> Test
testInitial = (1 ~=?) . initial

testFinal :: S -> Test
testFinal = ([3] ~=?) . final

testBlocks :: S -> Test
testBlocks stmt = sort expected ~=? sort actual
  where
    actual = blocks stmt
    expected =
      [ BlockAsg "x" (Num 5) 1,
        BlockAsg "y" (Num 1) 2,
        BlockCond BFalse 3,
        BlockAsg "y" (Add (AId "x") (AId "y")) 4,
        BlockAsg "x" (Add (AId "x") (Num 1)) 5
      ]

testLabels :: S -> Test
testLabels stmt = sort expected ~=? sort actual
  where
    actual = labels stmt
    expected = [1, 2, 3, 4, 5]

testFowardFlow :: S -> Test
testFowardFlow stmt = sort expected ~=? sort actual
  where
    actual = fflow stmt
    expected =
      [ (1, 2),
        (2, 3),
        (3, 4),
        (4, 5),
        (5, 3)
      ]

testBackwardFlow :: S -> Test
testBackwardFlow stmt = sort expected ~=? sort actual
  where
    actual = bflow stmt
    expected =
      [ (2, 1),
        (3, 2),
        (4, 3),
        (5, 4),
        (3, 5)
      ]

testRD :: S -> Test
testRD stmt = expected ~=? actual
  where
    actual = reachingDefinitions stmt
    expected =
      fromLists
        [ [],
          [],
          []
        ]
