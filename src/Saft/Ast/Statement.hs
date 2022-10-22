module Saft.Ast.Statement (Statement (..)) where

import Saft.Ast.Expression
import Saft.Ast.Internal

data Statement t
  = Function
      { identifier :: Identifier,
        arguments :: [(Identifier, t)],
        body :: [Statement t],
        returnType :: t
      }
  | Let
      { identifier :: Identifier,
        type_ :: t,
        expr :: Expression
      }
  | Return {expr :: Expression}
  deriving (Show, Eq)
