module Generator.CmdArgs where

import Options.Applicative

-- * Arguments Type

data Args a = Args
  { templateFile :: String,
    amount :: Maybe Int,
    debugOutput :: Bool,
    maxConfigurations :: Bool,
    interactive :: Bool,
    additional :: a
  }
  deriving (Show)

-- * Parser Info

parserInfo :: Parser a -> ParserInfo (Args a)
parserInfo additional = info (parser additional <**> helper) fullDesc

-- * Parsers

parser :: Parser a -> Parser (Args a)
parser addParser =
  Args
    <$> templateFileParser
    <*> optional amountParser
    <*> debugParser
    <*> maxParser
    <*> interactiveParser
    <*> addParser

templateFileParser :: Parser String
templateFileParser =
  strArgument $
    metavar "templateFile"
      <> action "file"
      <> help "Path to the template file"

amountParser :: Parser Int
amountParser =
  option auto $
    long "amount"
      <> short 'a'
      <> metavar "AMOUNT"
      <> help "Specify the amount of variants to generate"

debugParser :: Parser Bool
debugParser =
  switch $
    long "debug"
      <> short 'd'
      <> help "Output debug information"

maxParser :: Parser Bool
maxParser =
  switch $
    long "max"
      <> short 'm'
      <> help "Return the maximum amount of variants possible with the given template file"

interactiveParser :: Parser Bool
interactiveParser =
  switch $
    long "interactive"
      <> short 'i'
      <> help "Choose a value for each parameter manually"

-- * Execution function

executeParser :: Parser a -> IO (Args a)
executeParser additionalParser = execParser $ parserInfo additionalParser

emptyParser :: Parser ()
emptyParser = pure ()