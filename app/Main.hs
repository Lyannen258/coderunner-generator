module Main where

import CoderunnerGenerator.Interface
import CPPCoderunner.Generator
import CPPCoderunner.Parser

main :: IO ()
main = run parse generate