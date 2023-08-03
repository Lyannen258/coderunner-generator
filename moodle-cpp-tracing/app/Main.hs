module Main where

import Generator
import Generator.Moodle.CPPTracing.ArgParser (correctAmountParser)
import Generator.Moodle.CPPTracing.Generator
import Generator.Moodle.CPPTracing.Parser

main :: IO ()
main =
  runCustomCmdArgs
    (PF parse)
    (GFCstm generate)
    correctAmountParser