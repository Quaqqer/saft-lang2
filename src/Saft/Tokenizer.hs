module Saft.Tokenizer
  ( SToken (..),
    tokenizer,
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

identifier :: Parser (WithPos SToken)
identifier =
  try $
    lexeme $
      withPos $
        Identifier
          . T.pack
          <$> liftM2 (:) (lowerChar <|> char '_') (many alphaNumChar)

operatorChars :: Set.Set Char
operatorChars = Set.fromList "!#$%&*+./<=>?@\\^|-~:"

operator :: Parser (WithPos SToken)
operator = try $ lexeme $ withPos $ Operator . T.pack <$> some (satisfy (`Set.member` operatorChars))

tokensFromList :: [(T.Text, SToken)] -> [Parser (WithPos SToken)]
tokensFromList = map (\(s, t) -> withPos (t <$ string s))

keyword :: Parser (WithPos SToken)
keyword =
  choice
    ( map
        (\p -> lexeme (p <* notFollowedBy alphaNumChar))
        ( tokensFromList
            [ ("let", Let),
              ("return", Return),
              ("fn", Fn),
              ("void", TVoid),
              ("bool", TBool),
              ("int", TInt),
              ("float", TFloat),
              ("true", Bool True),
              ("false", Bool False)
            ]
        )
    )

symbols :: Parser (WithPos SToken)
symbols =
  choice
    ( map
        lexeme
        ( tokensFromList
            [ (":", Colon),
              (",", Comma),
              (";", Semicolon),
              (".", Dot),
              ("(", LParen),
              (")", RParen),
              ("{", LBrace),
              ("}", RBrace),
              ("->", Arrow)
            ]
            ++ [withPos (Equals <$ try (string "=" <* notFollowedBy "="))]
        )
    )

integer :: Parser (WithPos SToken)
integer = try $ lexeme $ withPos $ Int . T.pack <$> some numberChar

float :: Parser (WithPos SToken)
float = try $
  lexeme $
    withPos $ do
      i1 <- T.pack <$> some numberChar
      _ <- char '.'
      i2 <- T.pack <$> some numberChar
      return (Float $ i1 <> "." <> i2)

string_ :: Parser (WithPos SToken)
string_ = try $
  lexeme $
    withPos $ do
      _ <- char '"'
      str <-
        foldl (<>) ""
          <$> manyTill
            (string "\\\"" <|> (T.pack . singleton <$> satisfy (const True)))
            (string "\"")
      return $ String str

withPos :: Parser SToken -> Parser (WithPos SToken)
withPos p = do
  startPos <- getSourcePos
  startOffset <- getOffset
  tokenVal <- p
  endOffset <- getOffset
  endPos <- getSourcePos
  return $
    WithPos
      { tokenVal,
        startPos,
        endPos,
        tokenLength = endOffset - startOffset
      }

tokenizer :: Parser [WithPos SToken]
tokenizer = do
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

tokenize :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) TokenStream
tokenize fileName text = do
  tokens_ <- runParser tokenizer fileName text
  return $ TokenStream {streamInput = text, tokens = tokens_}
