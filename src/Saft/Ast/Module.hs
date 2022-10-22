module Saft.Ast.Module (Module (..)) where

import qualified Saft.Ast.Statement as Ast.Stmt

newtype Module t = Module {body :: [Ast.Stmt.Statement t]}
  deriving (Show, Eq)
