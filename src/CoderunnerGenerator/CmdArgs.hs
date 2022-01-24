module CoderunnerGenerator.CmdArgs where

import Options.Applicative

-- * Arguments Type

data Args = Args
  { templateFile :: String,
    amount :: Maybe Int
  }
  deriving (Show)

-- * Parser Info

parserInfo :: ParserInfo Args
parserInfo = info parser briefDesc

-- * Parsers

parser :: Parser Args
parser =
  Args
    <$> templateFileParser
    <*> optional amountParser

templateFileParser :: Parser String
templateFileParser =
  strArgument $
    metavar "templateFile"
      <> action "file"

amountParser :: Parser Int
amountParser =
  option auto $
    long "amount"
      <> short 'a'

-- * Execution function

executeParser :: IO Args
executeParser = execParser parserInfo