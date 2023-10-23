module MonotoneFramework () where

import AST (Block, Id, Label, Labeled (..), S)
import Data.Array (Array, array, (!), (//))
import DataFlow (Gen, Kill, blocks, genRD, killRD)
import Set (isSubsetOf, union, unique, (<++>), (<\\>))

-- a is the property space, the complete lattice of the framework
data MonotoneFramework a = MonotoneFramework
  { -- least upper bound operator
    -- combine :: [_L] -> _L,
    -- combination operators (union or intersection)
    combine :: [a] -> [a] -> [a],
    -- flowF or flowB
    flow :: [(Label, Label)],
    -- {init} or final
    extremalLabels :: [Label],
    -- specifies the initial or final analysis information
    extremalValues :: [a],
    -- transfer functions associated with the labels
    f :: TransferFunction a
  }
