module Saft.CliSpec (spec) where

import Options.Applicative
import Saft.Cli
import Test.Hspec

shouldParse :: Show a => Eq a => ParserResult a -> a -> Expectation
shouldParse result expected = case result of
  Success s -> s `shouldBe` expected
  Failure pFailure ->
    expectationFailure $
      "expected: " ++ show expected
        ++ "\n"
        ++ "but got: "
        ++ show pFailure
  CompletionInvoked _ -> error "Should not invoke completion"

shouldNotParse :: Show a => ParserResult a -> Expectation
shouldNotParse = \case
  Success s ->
    expectationFailure $
      "expected to fail parsing, but parsed: " ++ show s
  Failure _ -> return ()
  CompletionInvoked _ -> error "Should not invoke completion"

shouldParse' a b = parse cliArgsInfo a `shouldParse` b

shouldNotParse' = shouldNotParse . parse cliArgsInfo

defaultCliArgs = CliArgs {mainIs = "main", outputFile = Nothing}

parse :: ParserInfo a -> [String] -> ParserResult a
parse = execParserPure defaultPrefs

spec :: Spec
spec = describe "cli parsing" $ do
  it "parses a single module" $ do
    ["asd"] `shouldParse'` defaultCliArgs {modules = ["asd"]}

  it "parses multiple modules" $ do
    ["hello", "there"]
      `shouldParse'`
        defaultCliArgs {modules = ["hello", "there"]}

  it "fails when leaving out modules" $ do
    shouldNotParse' ["--main-s", "main#main"]

  it "parses changing the main function" $ do
    ["test", "--main-is", "test#main"]
      `shouldParse'`
        defaultCliArgs {modules = ["test"], mainIs = "test#main"}

  it "parses output file" $ do
    ["x", "--output-file", "asd"]
      `shouldParse'`
        defaultCliArgs {modules = ["x"], outputFile = Just "asd"}

  it "parses output file with shorthand" $ do
    ["x", "-o", "asd"]
      `shouldParse'`
        defaultCliArgs {modules = ["x"], outputFile = Just "asd"}
