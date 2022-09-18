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

parse :: ParserInfo a -> [String] -> ParserResult a
parse = execParserPure defaultPrefs

spec :: Spec
spec = describe "cli parsing" $ do
  it "parses modules" $ do
    parse cliArgsInfo ["asd"]
      `shouldParse` CliArgs {modules = ["asd"], mainIs = Nothing}
    parse cliArgsInfo ["hello", "there"]
      `shouldParse` CliArgs {modules = ["hello", "there"], mainIs = Nothing}
    parse cliArgsInfo ["hello", "there"]
      `shouldParse` CliArgs {modules = ["hello", "there"], mainIs = Nothing}

  it "fails when leaving out modules" $ do
    shouldNotParse $ parse cliArgsInfo []
    shouldNotParse $ parse cliArgsInfo ["--main-is", "main#main"]

  it "parses changing the main function" $ do
    parse cliArgsInfo ["test", "--main-is", "test#main"]
      `shouldParse` CliArgs {modules = ["test"], mainIs = Just "test#main"}
