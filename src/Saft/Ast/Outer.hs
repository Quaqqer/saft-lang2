module Saft.Ast.Outer (Outer (..)) where

import Saft.Ast.Internal
import Saft.Ast.Statement (Statement)
import Saft.Ast.Type (Type)

data Outer t
  = Function
      { name :: Identifier,
        args :: [(Identifier, Type)],
        body :: [Statement t],
        returnType :: t
      }
  | Import {path :: String, as :: Maybe Identifier}
  | ImportFrom {path :: String, vars :: [Identifier]}
  | Export {var :: Identifier}
  | ExportMany {vars :: [Identifier]}
