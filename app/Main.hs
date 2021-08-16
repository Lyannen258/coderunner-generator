module Main where

import Text.Parsec
import Parser
import Data.Tree(drawTree)

main :: IO ()
main = do
    fileContent <- readFile "../Input/writing_a_statement.txt"
    let output = parseFile fileContent
    writeFile "../Input/ast.txt" output

parseFile :: String -> String
parseFile content = case parse coderunnerParser "" content of
                Left a -> show a
                Right b -> drawTree $ toDataTree b