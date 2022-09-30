module Saft.Ast.Statement (Statement (..)) where

import Saft.Ast.Expression
import Saft.Ast.Internal
import Saft.Ast.Type

data Statement
  = Function
      { identifier :: Identifier,
        arguments :: [(Identifier, Type)],
        body :: [Statement],
        returnType :: Type
      }
  | Let
      { identifier :: Identifier,
        type_ :: Maybe Type,
        expr :: Expression
      }
  | Return {expr :: Expression}
  deriving (Show, Eq)
