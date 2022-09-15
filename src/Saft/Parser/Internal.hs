module Saft.Parser.Internal (Parser) where

import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void T.Text
