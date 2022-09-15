module Saft.Token (Token (..)) where

import qualified Data.Text as T

{- ORMOLU_DISABLE -}
data Token
  -- Keywords
  = Let
  | Fn

  -- Symbols
  | Colon      -- :
  | Semicolon  -- ;
  | Dot        -- .
  | LParen     -- (
  | RParen     -- )
  | LBrace     -- {
  | RBrace     -- }

  -- Identifiers and operators
  | Identifier T.Text
  | Operator T.Text

  -- Data
  | Integer T.Text
  | Float T.Text
  | String T.Text

  deriving (Show, Eq)
{- ORMOLU_ENABLE -}
