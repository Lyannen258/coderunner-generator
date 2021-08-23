module Main where

import Text.Parsec
import Parser
import Data.Tree(drawTree)
import System.IO
import System.FilePath (takeDirectory, takeBaseName)

main :: IO ()
main = do
    let listOfPath = map ("../Input/" ++) [
            "01_writing_a_statement.txt",
            "02_missing_parts_of_code.txt",
            "03_errors_in_the_code.txt"
            ]
    mapM_ parseFile listOfPath

parseFile :: String -> IO ()
parseFile filePath = do
    inputHandle <- openFile filePath ReadMode
    hSetEncoding inputHandle utf8
    fileContent <- hGetContents inputHandle

    let output = parseString fileContent

    let outputPath = takeDirectory filePath ++ "/AST_" ++ takeBaseName filePath
    outputHandle <- openFile outputPath WriteMode
    hSetEncoding outputHandle utf8
    hPutStr outputHandle output
    hClose outputHandle

parseString :: String -> String
parseString content = case parse coderunnerParser "" content of
                Left a -> show a
                Right b -> drawTree $ toDataTree b