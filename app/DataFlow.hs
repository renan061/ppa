module DataFlow where

import AST

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
fflow (Seq s1 s2) =
  let i = initial s2
   in fflow s1 ++ fflow s2 ++ [(f, i) | f <- final s1]
fflow (If l _ s1 s2) =
  fflow s1 ++ fflow s2 ++ [(l, initial s1), (l, initial s2)]
fflow (While l b s) =
  (l, initial s) : fflow s ++ [(f, l) | f <- final s]

bflow :: S -> [(Label, Label)]
bflow s = [(l2, l1) | (l1, l2) <- fflow s]
