module Saft.ParserSpec (spec) where

import Data.Text (Text)
import Saft.Ast.Module
import Saft.Ast.Statement
import Saft.Ast.Type as Ty
import Saft.Parser.Internal (module_)
import Saft.Token (TokenStream (TokenStream, streamInput, tokens))
import Saft.Tokenizer
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

shouldParseModule :: Text -> Module -> IO ()
shouldParseModule text m =
  let toks = parse tokenizer ""
      tokens_ = case toks text of
        Right tokens' -> tokens'
        Left _ ->
          error
            "Should be impossible since `shouldSucceedOn` succeeded before\
            \this evaluates."
      tokenStream = TokenStream {streamInput = text, tokens = tokens_}
   in do
        toks `shouldSucceedOn` text
        parse module_ "" tokenStream `shouldParse` m

spec :: Spec
spec = do
  describe "function parsing" $ do
    it "parses an empty function" $
      "fn main() {}"
        `shouldParseModule` Module
          [ Function
              { identifier = "main",
                arguments = [],
                body = [],
                returnType = Void
              }
          ]

    it "parses an empty function returning int" $
      "fn main() -> int {}"
        `shouldParseModule` Module
          [ Function
              { identifier = "main",
                arguments = [],
                body = [],
                returnType = Ty.Int
              }
          ]
