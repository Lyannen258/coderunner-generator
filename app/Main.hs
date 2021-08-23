module Main where

import Text.Parsec
import Parser
import Data.Tree(drawTree)
import System.IO

main :: IO ()
main = do
    inputHandle <- openFile "../Input/test.txt" ReadMode
    hSetEncoding inputHandle utf8
    fileContent <- hGetContents inputHandle

    let output = parseFile fileContent

    outputHandle <- openFile "../Input/ast.txt" WriteMode
    hSetEncoding outputHandle utf8
    hPutStr outputHandle output
    hClose outputHandle

parseFile :: String -> String
parseFile content = case parse coderunnerParser "" content of
                Left a -> show a
                Right b -> drawTree $ toDataTree b