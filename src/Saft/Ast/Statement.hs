module Saft.Ast.Statement (Statement (..)) where

import Saft.Ast.Internal
import Saft.Ast.Type

data Statement = Function
  { identifier :: Identifier,
    arguments :: [(Identifier, Type)],
    body :: [Statement],
    returnType :: Type
  }
  deriving (Show)
