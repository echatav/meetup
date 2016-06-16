module Expression where

data Expression
  = Literal Value
  | Variable String
  | UnOp UnOp Expression
  | BinOp BinOp Expression Expression
  | Let String Expression Expression
  deriving Show

data Value
  = VBool Bool
  | VInt Int
  | VDouble Double
  deriving Show

data UnOp
  = Not
  | Abs
  deriving Show

data BinOp
  = And
  | Or
  | Equ
  | Neq
  | Gt
  | Gte
  | Lt
  | Lte
  | Add
  | Sub
  | Mul
  deriving Show
