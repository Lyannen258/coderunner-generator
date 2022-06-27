module Main where

import Generator
import Generator.Moodle.CPPProgram.Generator
import Generator.Moodle.CPPProgram.Parser

main :: IO ()
main = run parse generate