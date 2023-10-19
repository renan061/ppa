module AST
  ( A (..),
    B (..),
    S (..),
    Block (..),
    Id,
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
  deriving (Eq, Ord)

data B -- boolean expressions
  = BTrue
  | BFalse
  | BId Id
  | Eq B B
  deriving (Eq, Ord)

data S -- statements
  = Skip Label
  | Asg Id A Label
  | Seq S S
  | If Label B S S
  | While Label B S
  deriving (Eq, Ord)

-------------------------------------------------------------------------------

instance Show A where
  show (Num n) = show n
  show (AId x) = x
  show (Add a1 a2) = show a1 ++ " + " ++ show a2

instance Show B where
  show BTrue = "true"
  show BFalse = "false"
  show (BId x) = x
  show (Eq b1 b2) = show b1 ++ " == " ++ show b2

instance Show S where
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

data Block
  = BlockSkip Label
  | BlockAsg Id A Label
  | BlockCond B Label
  deriving (Show, Eq, Ord)
