module Saft.Parser where

import Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

a :: Parser Text
a = string "hej"

b :: Parser Char
b = char 'a'
