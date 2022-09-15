module Saft.Parser.TokenizerSpec (spec) where

import Saft.Parser.Tokenizer
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "operator parsing" $ do
    it "parses operators" $ do
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

  describe "identifier parsing" $ do
    it "parses identifiers" $ do
      parse identifier "" "hello" `shouldParse` Identifier "hello"
      parse identifier "" "_hello" `shouldParse` Identifier "_hello"
      parse identifier "" "_1" `shouldParse` Identifier "_1"
      parse identifier "" "_" `shouldParse` Identifier "_"
      parse identifier "" "a1" `shouldParse` Identifier "a1"
    it "does not parse invalid identifiers" $ do
      parse identifier "" `shouldFailOn` "Asd"
      parse identifier "" `shouldFailOn` "X"
      parse identifier "" `shouldFailOn` "123"
      parse identifier "" `shouldFailOn` "1a"

  describe "float parsing" $ do
    it "parses floats" $ do
      parse float "" "123.2" `shouldParse` Float "123.2"
      parse float "" `shouldFailOn` "123"

  describe "general tokenization" $ do
    it "tokenizes streams" $
      let res1 = [Let, Identifier "x", Operator "=", Integer "3", Semicolon]
          res2 = [Let, Identifier "x", Operator "=", Float "3.3", Semicolon]
          res3 = [Let, Identifier "abc", Operator "=", String "hello there", Semicolon]
       in do
            parse tokenize "" "let x = 3;" `shouldParse` res1
            parse tokenize "" "let x = 3  ;" `shouldParse` res1
            parse tokenize "" "let x = 3.3;" `shouldParse` res2
            parse tokenize "" "let x = 3.3  ;" `shouldParse` res2
            parse tokenize "" "let abc = \"hello there\"  ;" `shouldParse` res3
