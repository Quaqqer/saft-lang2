module Saft.Parser.Tokenizer (Token (..), tokenizer) where

import Control.Monad (liftM2)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

data Token
  = Let
  | Identifier T.Text
  | Operator T.Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

identifier :: Parser Token
identifier = Identifier . T.pack <$> liftM2 (:) lowerChar (many alphaNumChar)

operatorChars :: Set.Set Char
operatorChars = Set.fromList "!#$%&*+./<=>?@\\^|-~:"

operator :: Parser Token
operator = Operator . T.pack <$> many (satisfy (`Set.member` operatorChars))

tokenizer :: Parser [Token]
tokenizer = do
  sc
  many
    ( foldl1
        (<|>)
        ( map
            (\(s, c) -> c <$ string s)
            [("let", Let)]
            ++ [operator, identifier]
        )
    )
