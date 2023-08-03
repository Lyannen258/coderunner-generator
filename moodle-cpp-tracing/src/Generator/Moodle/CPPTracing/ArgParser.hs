module Generator.Moodle.CPPTracing.ArgParser where

import Options.Applicative

correctAmountParser :: Parser (Maybe Int)
correctAmountParser =
  optional $
    option auto $
      long "correct-amount"
        <> short 'c'
        <> metavar "CORRECT-AMOUNT"
        <> help "Specify how many of the variants should be correct. This option can only be used with --amount. It is not compatible with --max and --interactive."