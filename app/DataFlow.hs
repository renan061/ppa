module DataFlow
  ( initial,
    labels,
    final,
    blocks,
    reachingDefinitions,
    fflow,
    bflow,
  )
where

import AST
import Data.Matrix (Matrix, matrix)
import qualified Data.Matrix as M
import Set (unique, (<++>), (<:>), (<\\>))

-------------------------------------------------------------------------------

-- question mark label
q :: Label
q = -1

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

blockLabel :: Block -> Label
blockLabel (BlockSkip l) = l
blockLabel (BlockAsg _ _ l) = l
blockLabel (BlockCond _ l) = l

blockLabels :: [Block] -> [Label]
blockLabels = unique . map blockLabel

labels :: S -> [Label]
labels = unique . map blockLabel . blocks

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

-- reaching definitions

killRD :: S -> Block -> [(Id, Label)]
killRD _ (BlockSkip _) = []
killRD s (BlockAsg x _ _) = (x, q) <:> [(x, l') | l' <- (f . blocks) s]
  where
    f = foldMap g
    g (BlockAsg x' _ l')
      | x == x' = [l']
      | otherwise = []
    g _ = []
killRD _ (BlockCond _ _) = []

genRD :: Block -> [(Id, Label)]
genRD (BlockSkip _) = []
genRD (BlockAsg x _ l) = [(x, l)]
genRD (BlockCond _ _) = []

entryRD :: S -> (Label -> [(Id, Label)]) -> Label -> [(Id, Label)]
entryRD s exitF l
  | l == initial s = unique [(x, q) | x <- fv s]
  | otherwise = (unique . concat) [exitF l1 | (l1, l2) <- fflow s, l == l2]

exitRD :: S -> (Label -> [(Id, Label)]) -> Label -> [(Id, Label)]
exitRD s entryF l = (entryF l <\\> kills) <++> gens
  where
    blocksL = [block | block <- blocks s, l == blockLabel block]
    kills = unique $ concatMap (killRD s) blocksL
    gens = unique $ concatMap genRD blocksL

-------------------------------------------------------------------------------

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x = if x == fx then x else fixpoint f fx
  where
    fx = f x

onceRD :: S -> Matrix [(Id, Label)] -> Matrix [(Id, Label)]
onceRD s m = M.mapPos f m
  where
    entry = entryRD s (\l -> M.getElem l 1 m)
    exit = exitRD s (\l -> M.getElem l 2 m)
    f (l, 1) _ = entry l
    f (l, 2) _ = exit l
    f _ v = v -- unreachable

reachingDefinitions :: S -> Matrix [(Id, Label)]
reachingDefinitions s = fixpoint (onceRD s) start
  where
    start = matrix (length $ labels s) 2 (const [])
