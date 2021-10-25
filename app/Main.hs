module Main where

import Data.Tree (drawTree)
import Generator
import Helper
import Interaction
import Parser
import SemanticAnalyzer
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeDirectory, (</>), takeExtension, dropExtension)
import System.IO
import System.Directory
import Text.Parsec

main :: IO ()
main = do
  args <- getArgs
  mapM_ analyzeFile args

analyzeFile :: String -> IO ()
analyzeFile filePath = do
  inputHandle <- openFile filePath ReadMode
  hSetEncoding inputHandle utf8
  fileContent <- hGetContents inputHandle

  createOutputDirectory filePath

  let parseResult = parseString fileContent
  writeParseResult filePath parseResult

  let semanticResult = parseToSemantic parseResult >>= semanticAnalysis
  writeSemanticResult filePath semanticResult

  valueResult <- case semanticResult of
    Right tbl -> questionUser tbl
    Left err -> return (Left err)

  let finalResult = do
        ast <- parseToSemantic parseResult
        st <- semanticResult
        vt <- valueResult
        generateOutput ast st vt filePath

  writeFinalResult filePath finalResult


-- Output Directory

createOutputDirectory :: FilePath -> IO ()
createOutputDirectory filePath = do
  createDirectoryIfMissing True $ dropExtension filePath

-- Parse Functions

writeParseResult :: String -> Either ParseError AST -> IO ()
writeParseResult filePath parseResult = do
  let astPath = takeDirectory filePath </> takeBaseName filePath ++ "/AST.txt"
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

-- Semantic Analysis Functions

writeSemanticResult :: String -> Either String SymbolTable -> IO ()
writeSemanticResult filePath result = do
  let stPath = takeDirectory filePath </> takeBaseName filePath ++ "/ST.txt"
  outputHandle <- openFile stPath WriteMode
  hSetEncoding outputHandle utf8
  hPutStr outputHandle $ semanticResultToString result
  hClose outputHandle

semanticResultToString :: Either String SymbolTable -> String
semanticResultToString (Left a) = a
semanticResultToString (Right b) = showSymbolTable b

-- Final result
writeFinalResult :: String -> Either String String -> IO ()
writeFinalResult filePath result =
  let output = case result of
        Left err -> err
        Right res -> res
   in do
        let resPath = takeDirectory filePath </> takeBaseName filePath ++ "/Res.xml"
        outputHandle <- openFile resPath WriteMode
        hSetEncoding outputHandle utf8
        hPutStr outputHandle output
        hClose outputHandle

-- Helper

parseToSemantic :: Either ParseError a -> Either String a
parseToSemantic (Left error) = Left "Error while Parsing"
parseToSemantic (Right a) = Right a
