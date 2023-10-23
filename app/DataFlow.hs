{-# LANGUAGE TupleSections #-}

module DataFlow (module DataFlow) where

import AST
import Data.Array (Array, array, (!), (//))
import Data.Matrix (Matrix, matrix)
import qualified Data.Matrix as M
import Set (isSubsetOf, union, unique, (<++>), (<:>), (<\\>))

-------------------------------------------------------------------------------

initial :: S -> Label
initial (Skip l) = l
initial (Asg _ _ l) = l
initial (Seq s _) = initial s
initial (If l _ _ _) = l
initial (While l _ _) = l

final :: S -> [Label]
final (Skip l) = [l]
final (Asg id _ l) = [l]
final (Seq _ s) = final s
final (If _ _ s1 s2) = final s1 <++> final s2
final (While l _ _) = [l]

blocks :: S -> [Block]
blocks (Skip l) = [BlockSkip l]
blocks (Asg x a l) = [BlockAsg x a l]
blocks (Seq s1 s2) = blocks s1 <++> blocks s2
blocks (If l b s1 s2) = BlockCond b l <:> blocks s1 <++> blocks s2
blocks (While l b s) = BlockCond b l <:> blocks s

fflow :: S -> [(Label, Label)]
fflow Skip {} = []
fflow Asg {} = []
fflow (Seq s1 s2) =
  fflow s1
    <++> fflow s2
    <++> [(l, initial s2) | l <- final s1]
fflow (If l _ s1 s2) =
  (l, initial s1)
    <:> (l, initial s2)
    <:> fflow s1
    <++> fflow s2
fflow (While l b s) =
  (l, initial s)
    <:> fflow s
    <++> [(l', l) | l' <- final s]

bflow :: S -> [(Label, Label)]
bflow s = unique [(l, l') | (l', l) <- fflow s]

-------------------------------------------------------------------------------

fvA :: A -> [Id]
fvA (AId x) = [x]
fvA (Add a1 a2) = fvA a1 <++> fvA a2
fvA _ = []

fvB :: B -> [Id]
fvB (BId x) = [x]
fvB (Eq b1 b2) = fvB b1 <++> fvB b2
fvB _ = []

fv :: S -> [Id]
fv (Asg x a _) = x <:> fvA a
fv (Seq s1 s2) = fv s1 <++> fv s2
fv (If _ b s1 s2) = fvB b <++> fv s1 <++> fv s2
fv (While _ b s) = fvB b <++> fv s
fv _ = []

-------------------------------------------------------------------------------

type Kill a = Block -> [a]

type Gen a = Block -> [a]

only :: [a] -> a
only [a] = a
only _ = error "list has more than one element"

killGen :: (Ord a) => Kill a -> Gen a -> [Block] -> Label -> [a] -> [a]
killGen kill gen blocks l x =
  let block = only [block | block <- blocks, l == label block]
   in (x <\\> kill block) <++> gen block

-------------------------------------------------------------------------------

killRD :: [Block] -> Kill (Id, Label)
killRD _ (BlockSkip _) = []
killRD allBlocks (BlockAsg x _ _) = (x, q) <:> foldMap f allBlocks
  where
    f (BlockAsg x' _ l')
      | x == x' = [(x, l')]
      | otherwise = []
    f _ = []
killRD _ (BlockCond _ _) = []

genRD :: Gen (Id, Label)
genRD (BlockSkip _) = []
genRD (BlockAsg x _ l) = [(x, l)]
genRD (BlockCond _ _) = []

-------------------------------------------------------------------------------

type Flow = [(Label, Label)]

type Worklist = Flow

type Analysis a = Array Int [a]

type TransferFunction a = Label -> [a] -> [a]

fTF :: (Ord a) => Kill a -> Gen a -> [Block] -> Label -> [a] -> [a]
fTF kill gen blocks l as = (as <\\> kills) <++> gens
  where
    bs = filter ((==) l . label) blocks
    kills = unique $ concatMap kill bs
    gens = unique $ concatMap gen bs

mfp1 :: [Label] -> [Label] -> [a] -> [a] -> Analysis a
mfp1 labels extremalLabels extremalValues bottom =
  array (1, length labels + 1) (map f labels)
  where
    f l = (l, if l `elem` extremalLabels then extremalValues else bottom)

mfp2 :: (Ord a) => Flow -> TransferFunction a -> Worklist -> Analysis a -> Analysis a
mfp2 _ _ [] analysis = analysis
mfp2 flow f ((l, l') : worklist) analysis
  -- TODO: isSubsetOf
  | f l (analysis ! l) `isSubsetOf` (analysis ! l') =
      mfp2 flow f worklist analysis
  | otherwise =
      let v' = analysis ! l' `union` f l (analysis ! l) -- TODO: union
          analysis = analysis // [(l', v')]
          worklist = [(l', l'') | (l_, l'') <- flow, l_ == l'] ++ worklist
       in mfp2 flow f worklist analysis

mfp3 :: (Ord a) => [Label] -> TransferFunction a -> Analysis a -> [([a], [a])]
mfp3 labels f analysis = map g [1 .. length labels]
  where
    g l = let x = analysis ! l in (x, f l x)

mfp labels flow f extremalLabels extremalValues bottom =
  let worklist = flow
      analysis0 = mfp1 labels extremalLabels extremalValues bottom
      analysis = mfp2 flow f worklist analysis0
   in mfp3 labels f analysis

-------------------------------------------------------------------------------

killGenRD blocks = killGen (killRD blocks) genRD blocks

flowRD = fflow

extremalLabelsRD s = [initial s]

extremalValuesRD s = map (,q) (fv s)

bottomRD = []

mfpRD :: S -> [([(Id, Label)], [(Id, Label)])]
mfpRD s =
  mfp
    labels
    (flowRD s)
    (killGenRD blocks')
    (extremalLabelsRD s)
    (extremalValuesRD s)
    bottomRD
  where
    blocks' = blocks s
    labels = map label blocks'
