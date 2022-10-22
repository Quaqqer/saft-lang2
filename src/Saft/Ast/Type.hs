module Saft.Ast.Type (Type (..)) where

data Type
  = -- | If the type is not specified
    Unknown
  | Void
  | Bool
  | Int
  | Float
  deriving (Show, Eq, Ord)
