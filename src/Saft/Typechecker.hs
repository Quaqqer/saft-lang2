module Saft.Typechecker () where

import Data.Map (Map)

data Type = Type

data Reference
  = GlobalFunction {ty :: Type}
  | LocalVar {ty :: Type}

data Namespace = Namespace {locals :: Map String Reference}

data TypecheckedModule = TypecheckedModule {globalNamespace :: Namespace}
