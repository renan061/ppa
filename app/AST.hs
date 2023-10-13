module AST
  ( A (..),
    B (..),
    S (..),
    Block (..),
    Label,
  )
where

type Label = Int

type Id = String

-------------------------------------------------------------------------------

data A -- arithmetic expressions
  = Num Int
  | AId Id
  | Add A A

data B -- boolean expressions
  = BTrue
  | BFalse
  | BId Id
  | Eq B B

data S -- statements
  = Empty
  | Skip Label
  | Asg Id A Label
  | Seq S S
  | If Label B S S
  | While Label B S

-------------------------------------------------------------------------------

instance Show A where
  show (Num n) = show n
  show (AId x) = x
  show (Add a1 a2) = show a1 ++ " = " ++ show a2

instance Show B where
  show BTrue = "true"
  show BFalse = "false"
  show (BId x) = x
  show (Eq b1 b2) = show b1 ++ " == " ++ show b2

instance Show S where
  show Empty = "_"
  show (Skip l) = "[skip]" ++ show l
  show (Asg x a l) = "[" ++ x ++ " = " ++ show a ++ "]" ++ show l
  show (Seq s1 s2) = show s1 ++ "; " ++ show s2
  show (If l b s1 s2) =
    "if ["
      ++ show b
      ++ "]"
      ++ show l
      ++ " { "
      ++ show s1
      ++ " } else { "
      ++ show s2
      ++ " }"
  show (While l b s) =
    "while ["
      ++ show b
      ++ "]"
      ++ show l
      ++ " { "
      ++ show s
      ++ " }"

-------------------------------------------------------------------------------

class Labels a where
  labels :: a -> [Label]

instance Labels S where
  labels (Skip l) = [l]
  labels (Asg _ _ l) = [l]
  labels (Seq s1 s2) = labels s1 ++ labels s2
  labels (If l b s1 s2) = l : labels s1 ++ labels s2
  labels (While l _ s) = l : labels s

data Block
  = Stmt S
  | Cond Label B

instance Labels Block where
  labels (Stmt s) = labels s
  labels (Cond l _) = [l]
