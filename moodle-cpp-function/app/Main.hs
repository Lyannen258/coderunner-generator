module Main where

import Generator
import Generator.Moodle.CPPFunction.Generator
import Generator.Moodle.CPPFunction.Parser

main :: IO ()
main = run (PF parse) (GF generate)