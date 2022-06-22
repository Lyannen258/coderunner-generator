module Main where

import Generator
import Generator.CPP.Moodle.Generator
import Generator.CPP.Moodle.Parser

main :: IO ()
main = run parse generate