module Saft.Cli (CliArgs (..), cliArgs, cliArgsInfo, cli) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import Saft.Tokenizer (tokenizer)
import Text.Megaparsec (runParser)

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
  args@CliArgs {modules} <- execParser cliArgsInfo

  files <-
    Map.fromList
      <$> mapM
        (\fileName -> (fileName,) . Text.pack <$> readFile (Text.unpack fileName))
        modules

  let tokens = Map.mapWithKey (runParser tokenizer . Text.unpack) files

  print tokens
