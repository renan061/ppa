module DataFlow (initial, final, blocks, fflow, bflow) where

import AST
import Data.Maybe (isJust, mapMaybe)

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
final (If _ _ s1 s2) = final s1 ++ final s2
final (While l _ _) = [l]

blocks :: S -> [Block]
blocks s@Skip {} = [Stmt s]
blocks s@Asg {} = [Stmt s]
blocks (Seq s1 s2) = blocks s1 ++ blocks s2
blocks (If l b s1 s2) = Cond l b : blocks s1 ++ blocks s2
blocks (While l b s) = Cond l b : blocks s

fflow :: S -> [(Label, Label)]
fflow Skip {} = []
fflow Asg {} = []
fflow (Seq s1 s2) = fflow s1 ++ fflow s2 ++ [(f, ini) | f <- final s1]
  where
    ini = initial s2
fflow (If l _ s1 s2) = fflow s1 ++ fflow s2 ++ [(l, ini1), (l, ini2)]
  where
    ini1 = initial s1
    ini2 = initial s2
fflow (While l b s) = (l, initial s) : fflow s ++ [(f, l) | f <- final s]

bflow :: S -> [(Label, Label)]
bflow s = [(l2, l1) | (l1, l2) <- fflow s]

-------------------------------------------------------------------------------

-- reaching definitions

killRD :: [Block] -> Block -> [(Id, Label)]
killRD blocks (Stmt Empty) = []
killRD blocks (Stmt (Skip l)) = []
killRD blocks (Stmt (Asg x a l)) = (x, q) : onlyAsgX x blocks
killRD blocks (Stmt (Seq s1 s2)) = []
killRD blocks (Stmt (If l b s1 s2)) = []
killRD blocks (Stmt (While l _ s)) = []
killRD blocks (Cond l b) = []

onlyAsg :: [Block] -> [Block]
onlyAsg = filter f
  where
    f (Stmt (Asg {})) = True
    f _ = False

onlyAsgX :: Id -> [Block] -> [(Id, Label)]
onlyAsgX x = mapMaybe f . onlyAsg
  where
    f (Stmt (Asg x' _ l))
      | x == x' = Just (x, l)
      | otherwise = Nothing
    f _ = Nothing
