module Saft.Cli (CliArgs (..), cliArgs, cliArgsInfo, cli) where

import Data.Text (Text)
import Options.Applicative

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
cliArgsInfo = info (cliArgs <**> helper) (fullDesc <> header "saft - the saft compiler")

cli :: IO ()
cli = do
  args <- execParser cliArgsInfo
  print args
