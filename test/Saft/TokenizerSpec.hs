module Saft.TokenizerSpec (spec) where

import qualified Data.Text as T
import Saft.Token
import Saft.Tokenizer
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

shouldParseTokens :: (HasCallStack, ShowErrorComponent e,  VisualStream s, TraversableStream s) => Either (ParseErrorBundle s e) [WithPos SToken] -> [SToken] -> Expectation
shouldParseTokens parseResult = shouldParse (map tokenVal <$> parseResult)

shouldParseToken :: (HasCallStack, ShowErrorComponent e,  VisualStream s, TraversableStream s) => Either (ParseErrorBundle s e) (WithPos SToken) -> SToken -> Expectation
shouldParseToken parseResult = shouldParse (tokenVal <$> parseResult)

spec :: Spec
spec = do
  describe "operator parsing" $ do
    it "parses operators" $ do
      parse operator "" "<$>" `shouldParseToken` Operator "<$>"
      parse operator "" "==" `shouldParseToken` Operator "=="
      parse operator "" "+" `shouldParseToken` Operator "+"
      parse operator "" "-" `shouldParseToken` Operator "-"
      parse operator "" "^" `shouldParseToken` Operator "^"
      parse operator "" "&&" `shouldParseToken` Operator "&&"
      parse operator "" "||" `shouldParseToken` Operator "||"
      parse operator "" "<" `shouldParseToken` Operator "<"

    it "does not parse invalid operators" $ do
      parse operator "" `shouldFailOn` ""
      parse operator "" `shouldFailOn` "and"
      parse operator "" `shouldFailOn` "or"

  describe "identifier parsing" $ do
    it "parses identifiers" $ do
      parse identifier "" "x" `shouldParseToken` Identifier "x"
      parse identifier "" "hello" `shouldParseToken` Identifier "hello"
      parse identifier "" "_hello" `shouldParseToken` Identifier "_hello"
      parse identifier "" "_1" `shouldParseToken` Identifier "_1"
      parse identifier "" "_" `shouldParseToken` Identifier "_"
      parse identifier "" "a1" `shouldParseToken` Identifier "a1"

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
      parse float "" "123.2" `shouldParseToken` Float "123.2"
      parse float "" `shouldFailOn` "123"

  describe "general tokenization" $ do
    it "tokenizes streams" $
      let res1 = [Let, Identifier "x", Equals, Integer "3", Semicolon]
          res2 = [Identifier "x", Operator "==", Float "3.3", Semicolon]
          res3 = [Let, Identifier "abc", Equals, String "hello there", Semicolon]
          res4 = [Identifier "leasd", Identifier "abc", Equals, String "hello there", Semicolon]
       in do
            parse tokenizer "" "let x = 3;" `shouldParseTokens` res1
            parse tokenizer "" "let x = 3  ;" `shouldParseTokens` res1
            parse tokenizer "" "x == 3.3;" `shouldParseTokens` res2
            parse tokenizer "" " x == 3.3  ;   " `shouldParseTokens` res2
            parse tokenizer "" "let abc = \"hello there\"  ;" `shouldParseTokens` res3
            parse tokenizer "" "leasd abc = \"hello there\"  ;" `shouldParseTokens` res4
            parse tokenizer "" "letasd" `shouldParseTokens` [Identifier "letasd"]
            parse tokenizer "" "let{}" `shouldParseTokens` [Let, LBrace, RBrace]
            parse tokenizer "" "{x}" `shouldParseTokens` [LBrace, Identifier "x", RBrace]

    it "skips comments" $ do
      parse tokenizer "" "// hello there" `shouldParseTokens` []
      parse tokenizer "" "let // hello there" `shouldParseTokens` [Let]
      parse tokenizer "" "/* hello there */" `shouldParseTokens` []
      parse tokenizer "" "/*/* hello there */*/" `shouldParseTokens` []
      parse tokenizer "" "let /*/* hello there */*/ x" `shouldParseTokens` [Let, Identifier "x"]

      let s1 =
            "let // let\n\
            \x" ::
              T.Text
      parse tokenizer "" s1 `shouldParseTokens` [Let, Identifier "x"]

      let s2 =
            "let /* let\n\
            \let\n\
            \*/\n\
            \x" ::
              T.Text
      parse tokenizer "" s2 `shouldParseTokens` [Let, Identifier "x"]
