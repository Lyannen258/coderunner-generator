module Main where

import Generator
import Generator.Moodle.CPPTracing.Generator
import Generator.Moodle.CPPTracing.Parser

main :: IO ()
main = run (PF parse) (GF generate)