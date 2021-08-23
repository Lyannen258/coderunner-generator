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

    let parseResult = parseString fileContent
    let astPath = takeDirectory filePath ++ "/AST_" ++ takeBaseName filePath
    writeParseResult astPath parseResult


writeParseResult :: String -> Either ParseError AST -> IO ()
writeParseResult filePath parseResult = do
    let astPath = takeDirectory filePath ++ "/AST_" ++ takeBaseName filePath
    outputHandle <- openFile astPath WriteMode
    hSetEncoding outputHandle utf8
    hPutStr outputHandle $ parseResultToString parseResult
    hClose outputHandle

parseString :: String -> Either ParseError AST
parseString = parse coderunnerParser ""

parseResultToString :: Either ParseError AST -> String
parseResultToString parseResult = case parseResult of
                            Left a -> show a
                            Right b -> drawTree $ toDataTree b