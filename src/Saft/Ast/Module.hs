module Saft.Ast.Module (Module (..)) where

import qualified Saft.Ast.Statement as Ast.Stmt

newtype Module = Module {body :: [Ast.Stmt.Statement]}
  deriving (Show)
