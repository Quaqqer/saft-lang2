module Saft.Tokenizer
  ( SToken (..),
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
import Saft.Token
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

-- symbol :: T.Text -> Parser T.Text
-- symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

identifier :: Parser SToken
identifier =
  try $
    lexeme $
      Identifier
        . T.pack
        <$> liftM2 (:) (lowerChar <|> char '_') (many alphaNumChar)

operatorChars :: Set.Set Char
operatorChars = Set.fromList "!#$%&*+./<=>?@\\^|-~:"

operator :: Parser SToken
operator = try $ lexeme $ Operator . T.pack <$> some (satisfy (`Set.member` operatorChars))

tokensFromList :: [(T.Text, SToken)] -> [Parser SToken]
tokensFromList = map (\(s, t) -> t <$ string s)

keyword :: Parser SToken
keyword =
  choice
    ( map
        (\p -> lexeme (p <* notFollowedBy alphaNumChar))
        ( tokensFromList
            [ ("let", Let),
              ("fn", Fn)
            ]
        )
    )

symbols :: Parser SToken
symbols =
  choice
    ( map lexeme $
        tokensFromList
          [ (":", Colon),
            (";", Semicolon),
            ("(", LParen),
            (")", RParen),
            ("{", LBrace),
            ("}", RBrace)
          ]
    )

integer :: Parser SToken
integer = try $ lexeme $ Integer . T.pack <$> some numberChar

float :: Parser SToken
float = try $
  lexeme $ do
    i1 <- T.pack <$> some numberChar
    _ <- char '.'
    i2 <- T.pack <$> some numberChar
    return (Float $ i1 <> "." <> i2)

string_ :: Parser SToken
string_ = try $
  lexeme $ do
    _ <- char '"'
    str <-
      foldl (<>) ""
        <$> manyTill
          (string "\\\"" <|> (T.pack . singleton <$> satisfy (const True)))
          (string "\"")
    return $ String str

tokenize :: Parser [SToken]
tokenize = do
  sc

  parsedTokens <-
    many $
      try keyword
        <|> symbols
        <|> identifier
        <|> operator
        <|> float
        <|> integer
        <|> string_
  eof

  return parsedTokens
