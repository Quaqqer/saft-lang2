{-# LANGUAGE TypeFamilies #-}

module Saft.Token
  ( SToken (..),
    WithPos (..),
    TokenStream (..),
  )
where

import Data.Data (Proxy (..))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec hiding (tokens)

{- ORMOLU_DISABLE -}
data SToken
  -- Keywords
  = Let
  | Return
  | Fn

  -- Symbols
  | Colon      -- :
  | Comma      -- ,
  | Semicolon  -- ;
  | Dot        -- .
  | LParen     -- (
  | RParen     -- )
  | LBrace     -- {
  | RBrace     -- }
  | Equals     -- =
  | Arrow      -- ->

  -- Identifiers and operators
  | Identifier String
  | Operator String

  -- Types
  | TVoid -- Special case, both a type and a value
  | TBool
  | TInt
  | TFloat

  -- Data
  | Bool Bool
  | Int String
  | Float String
  | String String

  deriving (Show, Ord, Eq)
{- ORMOLU_ENABLE -}

data WithPos a = WithPos
  { startPos :: SourcePos,
    endPos :: SourcePos,
    tokenLength :: Int,
    tokenVal :: a
  }
  deriving (Eq, Ord, Show)

data TokenStream = TokenStream
  { streamInput :: String,
    tokens :: [WithPos SToken]
  }
  deriving (Show)

instance Stream TokenStream where
  type Token TokenStream = WithPos SToken
  type Tokens TokenStream = [WithPos SToken]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream str (t : ts)) =
    Just
      ( t,
        TokenStream (drop (tokensLength pxy (t :| [])) str) ts
      )
  takeN_ n (TokenStream str s)
    | n <= 0 = Just ([], TokenStream str s)
    | null s = Nothing
    | otherwise =
      let (x, s') = splitAt n s
       in case NE.nonEmpty x of
            Nothing -> Just (x, TokenStream str s')
            Just nex -> Just (x, TokenStream (drop (tokensLength pxy nex) str) s')
  takeWhile_ f (TokenStream str s) =
    let (x, s') = List.span f s
     in case NE.nonEmpty x of
          Nothing -> (x, TokenStream str s')
          Just nex -> (x, TokenStream (drop (tokensLength pxy nex) str) s')

instance VisualStream TokenStream where
  showTokens Proxy =
    unwords
      . NE.toList
      . fmap (show . tokenVal)
  tokensLength Proxy xs =
    sum (tokenLength <$> xs)

instance TraversableStream TokenStream where
  reachOffset o PosState {..} =
    ( Just (prefix <> restOfLine),
      PosState
        { pstateInput =
            TokenStream
              { streamInput = postStr,
                tokens = post
              },
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
        }
    )
    where
      prefix =
        if sameLine
          then pstateLinePrefix <> preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> pstateSourcePos
          (x : _) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) (tokens pstateInput)
      (preStr, postStr) = splitAt tokensConsumed (streamInput pstateInput)
      preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy TokenStream
pxy = Proxy
