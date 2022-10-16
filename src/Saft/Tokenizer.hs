-- |
-- Module      : Saft.Tokenizer
-- Description : Convert text into token streams.
--
-- This module provides methods for tokenizing text into a token stream.
module Saft.Tokenizer
  ( -- * Primitive Parsers

    -- | The primitive parsers that are used to create the final tokenizer.
    operator,
    identifier,
    keyword,
    symbols,
    float,
    integer,

    -- * Final Tokenization

    -- | The final tokenizers that tokenize a stream of text into streams of
    -- tokens.
    tokenizer,
    tokenize,
  )
where

import Control.Monad (liftM2)
import Data.List (singleton)
import qualified Data.Set as Set
import Data.Void (Void)
import Saft.Token
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | The space and comment consumer.
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

-- | Make a parser consume spaces after it
lexeme ::
  -- | The parser to add the space consumer to
  Parser a ->
  Parser a
lexeme = L.lexeme sc

-- | Parse an identifier into a token
identifier :: Parser (WithPos SToken)
identifier =
  try $
    lexeme $
      withPos $
        Identifier
          <$> liftM2 (:) (lowerChar <|> char '_') (many alphaNumChar)

-- | The set of characters used in custom operators
operatorChars :: Set.Set Char
operatorChars = Set.fromList "!#$%&*+./<=>?@\\^|-~:"

-- | Parse an operator
operator :: Parser (WithPos SToken)
operator = try $ lexeme $ withPos $ Operator <$> some (satisfy (`Set.member` operatorChars))

-- | Parse a keyword
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

-- | Parse symbols
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

-- | Parse a single integer
integer :: Parser (WithPos SToken)
integer = try $ lexeme $ withPos $ Int <$> some numberChar

-- | Parse a single floating point number
float :: Parser (WithPos SToken)
float = try $
  lexeme $
    withPos $ do
      i1 <- some numberChar
      _ <- char '.'
      i2 <- some numberChar
      return (Float $ i1 <> "." <> i2)

-- | Parse a string
string_ :: Parser (WithPos SToken)
string_ = try $
  lexeme $
    withPos $ do
      _ <- char '"'
      str <-
        foldl (<>) ""
          <$> manyTill
            (string "\\\"" <|> (singleton <$> satisfy (const True)))
            (string "\"")
      return $ String str

-- | Add position to a token parser
withPos ::
  -- | The parser to add position to
  Parser SToken ->
  Parser (WithPos SToken)
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

-- | Make many parsers from a list
tokensFromList ::
  -- | A list of text and its corresponding token
  [(String, SToken)] ->
  [Parser (WithPos SToken)]
tokensFromList = map (\(s, t) -> withPos (t <$ string s))

-- | The parser that tokenizes a stream of text into multiple tokens
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

-- | Tokenize text into a final `TokenStream`
tokenize ::
  -- | The file name
  String ->
  -- | The text to parse
  String ->
  Either (ParseErrorBundle String Void) TokenStream
tokenize fileName text = do
  tokens_ <- runParser tokenizer fileName text
  return $ TokenStream {streamInput = text, tokens = tokens_}
