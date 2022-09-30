module Saft.Ast.Expression (Expression (..)) where

import Data.Text (Text)

data Expression
  = Void
  | Bool Bool
  | Int Text
  | Float Text
  deriving (Show, Eq)
