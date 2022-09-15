module Saft.Parser.Tokenizer
  ( Token (..),
    tokenize,
    operator,
    identifier,
    keyword,
    symbols,
    float,
    integer,
  )
where

import Control.Monad (liftM2)
import Data.List (singleton)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

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

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

identifier :: Parser Token
identifier =
  try $
    lexeme $
      Identifier
        . T.pack
        <$> liftM2 (:) (lowerChar <|> char '_') (many alphaNumChar)

operatorChars :: Set.Set Char
operatorChars = Set.fromList "!#$%&*+./<=>?@\\^|-~:"

operator :: Parser Token
operator = try $ lexeme $ Operator . T.pack <$> some (satisfy (`Set.member` operatorChars))

tokensFromList :: [(T.Text, Token)] -> Parser Token
tokensFromList l = foldl1 (<|>) (map (\(s, t) -> t <$ symbol s) l)

keyword :: Parser Token
keyword =
  tokensFromList
    [ ("let", Let),
      ("fn", Fn)
    ]

symbols :: Parser Token
symbols =
  tokensFromList
    [ (":", Colon),
      (";", Semicolon),
      ("(", LParen),
      (")", RParen),
      ("{", LBrace),
      ("}", LBrace)
    ]

integer :: Parser Token
integer = try $ lexeme $ Integer . T.pack <$> some numberChar

float :: Parser Token
float = try $
  lexeme $ do
    i1 <- T.pack <$> some numberChar
    _ <- char '.'
    i2 <- T.pack <$> some numberChar
    return (Float $ i1 <> "." <> i2)

string_ :: Parser Token
string_ = try $
  lexeme $ do
    _ <- char '"'
    str <-
      foldl (<>) ""
        <$> manyTill
          (string "\\\"" <|> (T.pack . singleton <$> satisfy (const True)))
          (string "\"")
    return $ String str

tokenize :: Parser [Token]
tokenize = do
  sc
  many $
    keyword
      <|> symbols
      <|> identifier
      <|> operator
      <|> float
      <|> integer
      <|> string_
