module Saft.TokenizerSpec (spec) where

import Saft.Token
import Saft.Tokenizer
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

shouldParseTokens ::
  (HasCallStack, ShowErrorComponent e, VisualStream s, TraversableStream s) =>
  Either (ParseErrorBundle s e) [WithPos SToken] ->
  [SToken] ->
  Expectation
shouldParseTokens parseResult = shouldParse (map tokenVal <$> parseResult)

shouldParseToken ::
  (HasCallStack, ShowErrorComponent e, VisualStream s, TraversableStream s) =>
  Either (ParseErrorBundle s e) (WithPos SToken) ->
  SToken ->
  Expectation
shouldParseToken parseResult = shouldParse (tokenVal <$> parseResult)

shouldTokenize :: String -> [SToken] -> Expectation
shouldTokenize text tokens_ = parse tokenizer "" text `shouldParseTokens` tokens_

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

    it "skips comments" $ do
      "// hello there" `shouldTokenize` []
      "let // hello there" `shouldTokenize` [Let]
      "/* hello there */" `shouldTokenize` []
      "/*/* hello there */*/" `shouldTokenize` []
      "let /*/* hello there */*/ x" `shouldTokenize` [Let, Identifier "x"]

      "let // let\n\
      \x"
        `shouldTokenize` [Let, Identifier "x"]

      "let /* let\n\
      \let\n\
      \*/\n\
      \x"
        `shouldTokenize` [Let, Identifier "x"]

  describe "general tokenization" $ do
    it "tokenizes streams" $
      let res1 = [Let, Identifier "x", Equals, Int "3", Semicolon]
          res2 = [Identifier "x", Operator "==", Float "3.3", Semicolon]
          res3 = [Let, Identifier "abc", Equals, String "hello there", Semicolon]
          res4 = [Identifier "leasd", Identifier "abc", Equals, String "hello there", Semicolon]
       in do
            "let x = 3;" `shouldTokenize` res1
            "let x = 3  ;" `shouldTokenize` res1

            "x == 3.3;" `shouldTokenize` res2
            " x == 3.3  ;   " `shouldTokenize` res2

            "let abc = \"hello there\"  ;" `shouldTokenize` res3

            "leasd abc = \"hello there\"  ;" `shouldTokenize` res4

            "letasd" `shouldTokenize` [Identifier "letasd"]

            "let{}" `shouldTokenize` [Let, LBrace, RBrace]

            "{x}" `shouldTokenize` [LBrace, Identifier "x", RBrace]

            "fn main() -> int {}"
              `shouldTokenize` [ Fn,
                                 Identifier "main",
                                 LParen,
                                 RParen,
                                 Arrow,
                                 TInt,
                                 LBrace,
                                 RBrace
                               ]
