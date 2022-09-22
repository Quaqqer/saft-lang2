module Saft.Cli (CliArgs (..), cliArgs, cliArgsInfo, cli) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import qualified Saft.Tokenizer as Tokenizer
import Text.Megaparsec (errorBundlePretty)

data CliArgs = CliArgs
  { modules :: [Text],
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

  files <-
    Map.fromList
      <$> mapM
        (\fileName -> (fileName,) . Text.pack <$> readFile (Text.unpack fileName))
        modules

  let tokens = Map.mapWithKey (Tokenizer.tokenize . Text.unpack) files

  mapM_
    ( ( \case
          Right r -> print r
          Left err -> putStrLn $ errorBundlePretty err
      )
        . snd
    )
    (Map.toList tokens)
