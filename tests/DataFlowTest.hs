module Main (main) where

import AST (A (..), B (..), Block (..), S)
import Data.Function ((&))
import Data.List (sort)
import DataFlow
import Parser (parseOrFail)
import Test.HUnit (Test (..), assertFailure, runTestTTAndExit, (~=?))

stmt =
  parseOrFail
    "[x = 5]1; [y = 1]2; while [false]3 { [y = x + y]4; [x = x + 1]5 }"

main =
  do
    print stmt
    (runTestTTAndExit . TestList . map (stmt &))
      [ testInitial,
        testFinal,
        testBlocks,
        testFowardFlow,
        testBackwardFlow,
        testFV,
        testKillRD,
        testGenRD,
        testRD
      ]

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

testFV :: S -> Test
testFV stmt = sort expected ~=? sort actual
  where
    actual = fv stmt
    expected = ["x", "y"]

testKillRD :: S -> Test
testKillRD stmt = TestList (map f cases)
  where
    f (block, expected) = sort expected ~=? sort (killRD (blocks stmt) block)
    cases =
      [ (BlockAsg "x" (Num 5) 1, [("x", -1), ("x", 1), ("x", 5)]),
        (BlockAsg "y" (Num 1) 2, [("y", -1), ("y", 2), ("y", 4)]),
        (BlockCond BFalse 3, []),
        (BlockAsg "y" (Add (AId "x") (AId "y")) 4, [("y", -1), ("y", 2), ("y", 4)]),
        (BlockAsg "x" (Add (AId "x") (Num 1)) 5, [("x", -1), ("x", 1), ("x", 5)])
      ]

testGenRD :: S -> Test
testGenRD _ = TestList (map f cases)
  where
    f (block, expected) = sort expected ~=? sort (genRD block)
    cases =
      [ (BlockAsg "x" (Num 5) 1, [("x", 1)]),
        (BlockAsg "y" (Num 1) 2, [("y", 2)]),
        (BlockCond BFalse 3, []),
        (BlockAsg "y" (Add (AId "x") (AId "y")) 4, [("y", 4)]),
        (BlockAsg "x" (Add (AId "x") (Num 1)) 5, [("x", 5)])
      ]

testRD :: S -> Test
testRD stmt = expected ~=? actual
  where
    actual = mfpRD stmt
    expected = []
