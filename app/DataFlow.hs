{-# LANGUAGE TupleSections #-}

module DataFlow () where

import AST
import Data.Set (Set, insert, singleton, (\\))
import qualified Data.Set as Set
import Data.Tuple (swap)

-- question mark label
q :: Label
q = -1

(<+>) :: (Ord a) => a -> Set a -> Set a
(<+>) = insert

(<++>) :: (Semigroup a) => a -> a -> a
(<++>) = (<>)

infixr 5 <+>

-------------------------------------------------------------------------------

initial :: S -> Label
initial (Skip l) = l
initial (Asg _ _ l) = l
initial (Seq s _) = initial s
initial (If l _ _ _) = l
initial (While l _ _) = l

final :: S -> Set Label
final (Skip l) = singleton l
final (Asg id _ l) = singleton l
final (Seq _ s) = final s
final (If _ _ s1 s2) = final s1 <++> final s2
final (While l _ _) = singleton l

blocks :: S -> Set Block
blocks (Skip l) = singleton (BlockSkip l)
blocks (Asg x a l) = singleton (BlockAsg x a l)
blocks (Seq s1 s2) = blocks s1 <++> blocks s2
blocks (If l b s1 s2) = BlockCond b l <+> blocks s1 <++> blocks s2
blocks (While l b s) = BlockCond b l <+> blocks s

blockLabel :: Block -> Label
blockLabel (BlockSkip l) = l
blockLabel (BlockAsg _ _ l) = l
blockLabel (BlockCond _ l) = l

blockLabels :: Set Block -> Set Label
blockLabels = Set.map blockLabel

labels :: S -> Set Label
labels s = Set.map blockLabel (blocks s)

fflow :: S -> Set (Label, Label)
fflow Skip {} = Set.empty
fflow Asg {} = Set.empty
fflow (Seq s1 s2) = fflow s1 <++> fflow s2 <++> edges
  where
    edges = Set.map (,initial s2) (final s1)
fflow (If l _ s1 s2) = fflow s1 <++> fflow s2 <++> edges
  where
    edges = (l, initial s2) <+> (l, initial s1) <+> Set.empty
fflow (While l b s) = (l, initial s) <+> fflow s <++> edges
  where
    edges = Set.map (,l) (final s)

bflow :: S -> Set (Label, Label)
bflow s = Set.map swap (fflow s)

-------------------------------------------------------------------------------

fvA :: A -> Set Id
fvA (AId x) = singleton x
fvA (Add a1 a2) = fvA a1 <++> fvA a2
fvA _ = Set.empty

fvB :: B -> Set Id
fvB (BId x) = singleton x
fvB (Eq b1 b2) = fvB b1 <++> fvB b2
fvB _ = Set.empty

fv :: S -> Set Id
fv (Asg x a _) = singleton x <++> fvA a
fv (Seq s1 s2) = fv s1 <++> fv s2
fv (If _ b s1 s2) = fvB b <++> fv s1 <++> fv s2
fv (While _ b s) = fvB b <++> fv s
fv _ = Set.empty

-------------------------------------------------------------------------------

-- reaching definitions

killRD :: Set Block -> Block -> Set (Id, Label)
killRD blocks (BlockSkip _) = Set.empty
killRD blocks (BlockAsg x _ _) = (x, q) <+> Set.map (x,) (f blocks)
  where
    f = foldMap g
    g (BlockAsg x' _ l)
      | x == x' = singleton l
      | otherwise = Set.empty
    g _ = Set.empty

genRD :: Block -> Set (Id, Label)
genRD (BlockSkip _) = Set.empty
genRD (BlockAsg x _ l) = singleton (x, l)
genRD (BlockCond _ _) = Set.empty

entryRD :: S -> Label -> Set (Id, Label)
entryRD s l
  | l == initial s = Set.map (,q) (fv s)
  | otherwise = foldMap (exitRD s) exits
  where
    exits = Set.map fst (Set.filter ((==) l . snd) (fflow s))

exitRD :: S -> Label -> Set (Id, Label)
exitRD s l = set1 <++> set2
  where
    set1 = foldMap (\block -> entryRDl \\ killRD bs block) bs
    set2 = foldMap genRD bs

    entryRDl = entryRD s l

    bs = blocks s
    bl = Set.filter (l ==) (blockLabels bs)
