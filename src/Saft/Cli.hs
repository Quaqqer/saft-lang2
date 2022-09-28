module Saft.Cli (CliArgs (..), cliArgs, cliArgsInfo, cli) where

import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import qualified Saft.Parser.Internal as SP
import qualified Saft.Tokenizer as Tokenizer
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty, runParser)

data CliArgs = CliArgs
  { modules :: [String],
    mainIs :: Maybe Text
  }
  deriving (Show, Eq)

cliArgs :: Parser CliArgs
cliArgs =
  CliArgs
    <$> some
      ( strArgument
          ( help "The module(s) to compile" <> metavar "MODULES..."
          )
      )
    <*> optional
      ( strOption
          ( long "main-is"
              <> help "The main function of the program"
          )
      )

cliArgsInfo :: ParserInfo CliArgs
cliArgsInfo =
  info (cliArgs <**> helper) (fullDesc <> header "saft - the saft compiler")

cli :: IO ()
cli = do
  CliArgs {modules} <- execParser cliArgsInfo

  moduleFiles <-
    mapM
      (\fileName -> (fileName,) . Text.pack <$> readFile fileName)
      modules

  moduleTokens <-
    mapM
      ( \(fn, text) ->
          let mt = Tokenizer.tokenize fn text
           in case mt of
                Right tokens -> return (fn, tokens)
                Left errBundle -> do
                  putStrLn ("Could not parse module " ++ fn ++ ".\n")
                  putStrLn $ errorBundlePretty errBundle
                  exitFailure
      )
      moduleFiles

  modules_ <-
    mapM
      ( \(fn, tokens) ->
          let mModule_ = runParser SP.module_ fn tokens
           in case mModule_ of
                Right module_ -> return (fn, module_)
                Left errBundle -> do
                  putStrLn $ errorBundlePretty errBundle
                  exitFailure
      )
      moduleTokens

  print modules_
