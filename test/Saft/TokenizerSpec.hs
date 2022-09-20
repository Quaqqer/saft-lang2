module Saft.TokenizerSpec (spec) where

import qualified Data.Text as T
import Saft.Token
import Saft.Tokenizer
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
      parse identifier "" "x" `shouldParse` Identifier "x"
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

  describe "keyword parsing" $ do
    it "does not parse identifiers as keywords" $ do
      parse keyword "" `shouldSucceedOn` "let"
      parse keyword "" `shouldFailOn` "lethello"

    it "does not go past spaces" $ do
      parse keyword "" `shouldSucceedOn` "let let"

  describe "float parsing" $ do
    it "parses floats" $ do
      parse float "" "123.2" `shouldParse` Float "123.2"
      parse float "" `shouldFailOn` "123"

  describe "general tokenization" $ do
    it "tokenizes streams" $
      let res1 = [Let, Identifier "x", Equals, Integer "3", Semicolon]
          res2 = [Identifier "x", Operator "==", Float "3.3", Semicolon]
          res3 = [Let, Identifier "abc", Equals, String "hello there", Semicolon]
          res4 = [Identifier "leasd", Identifier "abc", Equals, String "hello there", Semicolon]
       in do
            parse tokenizer "" "let x = 3;" `shouldParse` res1
            parse tokenizer "" "let x = 3  ;" `shouldParse` res1
            parse tokenizer "" "x == 3.3;" `shouldParse` res2
            parse tokenizer "" " x == 3.3  ;   " `shouldParse` res2
            parse tokenizer "" "let abc = \"hello there\"  ;" `shouldParse` res3
            parse tokenizer "" "leasd abc = \"hello there\"  ;" `shouldParse` res4
            parse tokenizer "" "letasd" `shouldParse` [Identifier "letasd"]
            parse tokenizer "" "let{}" `shouldParse` [Let, LBrace, RBrace]
            parse tokenizer "" "{x}" `shouldParse` [LBrace, Identifier "x", RBrace]

    it "skips comments" $ do
      parse tokenizer "" "// hello there" `shouldParse` []
      parse tokenizer "" "let // hello there" `shouldParse` [Let]
      parse tokenizer "" "/* hello there */" `shouldParse` []
      parse tokenizer "" "/*/* hello there */*/" `shouldParse` []
      parse tokenizer "" "let /*/* hello there */*/ x" `shouldParse` [Let, Identifier "x"]

      let s1 =
            "let // let\n\
            \x" ::
              T.Text
      parse tokenizer "" s1 `shouldParse` [Let, Identifier "x"]

      let s2 =
            "let /* let\n\
            \let\n\
            \*/\n\
            \x" ::
              T.Text
      parse tokenizer "" s2 `shouldParse` [Let, Identifier "x"]
