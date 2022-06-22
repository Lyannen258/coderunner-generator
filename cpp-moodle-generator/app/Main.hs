module Main where

import Generator.Interface
import CPPCoderunner.Generator
import CPPCoderunner.Parser

main :: IO ()
main = run parse generate