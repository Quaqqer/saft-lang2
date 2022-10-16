module Saft.Ast.Expression (Expression (..)) where

data Expression
  = Void
  | Bool Bool
  | Int String
  | Float String
  deriving (Show, Eq)
