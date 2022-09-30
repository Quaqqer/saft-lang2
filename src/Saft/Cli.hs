module Saft.Cli (CliArgs (..), cliArgs, cliArgsInfo, cli) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (..))
import Options.Applicative
import Saft.Compiler (compileIR, generateIR)
import Saft.Jit (runJit)
import qualified Saft.Parser.Statement as SP
import qualified Saft.Tokenizer as Tokenizer
import System.Exit (exitFailure, exitWith)
import Text.Megaparsec (errorBundlePretty, runParser)

data CliArgs = CliArgs
  { modules :: [String],
    mainIs :: Text,
    outputFile :: Maybe String
  }
  deriving (Show, Eq)

cliArgs :: Parser CliArgs
cliArgs =
  CliArgs
    <$> some
      ( strArgument
          ( help "The module(s) to compile"
              <> metavar "MODULES..."
          )
      )
    <*> strOption
      ( long "main-is"
          <> metavar "FUNCTION"
          <> help "The main function of the program"
          <> value "main"
      )
    <*> optional
      ( strOption
          ( long "output-file"
              <> short 'o'
              <> metavar "OUTPUT"
              <> help "Output file path"
          )
      )

cliArgsInfo :: ParserInfo CliArgs
cliArgsInfo =
  info (cliArgs <**> helper) (fullDesc <> header "saft - the saft compiler")

cli :: IO ()
cli = do
  CliArgs
    { modules,
      mainIs,
      outputFile
    } <-
    execParser cliArgsInfo

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
          let mModule_ = runParser SP.pModule fn tokens
           in case mModule_ of
                Right module_ -> return (fn, module_)
                Left errBundle -> do
                  putStrLn $ errorBundlePretty errBundle
                  exitFailure
      )
      moduleTokens

  let [(fn, mod)] = modules_ -- TODO: Compile multiple modules
  let ir = generateIR fn mainIs mod

  case outputFile of
    Nothing -> do
      -- Run the jit
      Just exitCode <- runJit ir
      exitWith $
        if exitCode == 0
          then ExitSuccess
          else ExitFailure exitCode
    Just f -> compileIR f ir
