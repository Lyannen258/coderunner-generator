module Main where

import Text.Parsec
import Parser

main :: IO ()
main = do
    fileContent <- readFile "../Input/writing_a_statement.txt"
    let output = parseFile fileContent
    print output

parseFile :: String -> String
parseFile content = case parse coderunnerParser "" content of
                Left a -> show a
                Right b -> show b