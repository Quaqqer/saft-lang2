module Saft.Ast.Expression (Expression (..)) where

import Saft.Ast.Internal

data Expression
  = Void
  | Bool Bool
  | Int String
  | Float String
  | Var Identifier
  | Call { identifier :: Identifier, arguments :: [Expression]}
  deriving (Show, Eq)
