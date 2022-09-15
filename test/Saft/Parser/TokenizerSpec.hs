module Saft.Parser.TokenizerSpec (spec) where

import Saft.Parser.Tokenizer
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec :: Spec
spec = do
  it "tokenizes operators" $ do
    parse operator "" "<$>" `shouldParse` Operator "<$>"
    parse operator "" "==" `shouldParse` Operator "=="
    parse operator "" "+" `shouldParse` Operator "+"
    parse operator "" "-" `shouldParse` Operator "-"
    parse operator "" "^" `shouldParse` Operator "^"
    parse operator "" "&&" `shouldParse` Operator "&&"
    parse operator "" "||" `shouldParse` Operator "||"
    parse operator "" "<" `shouldParse` Operator "<"
  it "does not parse invalid operators" $ do
    parse operator "" `shouldFailOn` ""
    parse operator "" `shouldFailOn` "and"
    parse operator "" `shouldFailOn` "or"
