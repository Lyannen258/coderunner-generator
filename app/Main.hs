module Main where

import Control.Monad.Trans.Except (runExceptT)
import Data.Tree (drawTree)
import CoderunnerGenerator.Generator
import CoderunnerGenerator.Helper
import CoderunnerGenerator.Interaction
import CoderunnerGenerator.Parser
import qualified CoderunnerGenerator.Types.AbstractSyntaxTree as AST
import qualified CoderunnerGenerator.SemanticAnalyzer as SA
import qualified CoderunnerGenerator.Types.SymbolTable as ST
import qualified CoderunnerGenerator.Types.ConstraintGraph as CG
import System.Directory
import System.Environment (getArgs)
import System.FilePath (dropExtension, takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO
import Text.Parsec
import Text.Pretty.Simple (pShowNoColor)
import Data.Text.Lazy (unpack)

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

  let semanticResult = parseToSemantic parseResult >>= SA.semanticAnalysis
  writeSemanticResult filePath semanticResult

  valueResult <- case semanticResult of
    Right tbl -> runExceptT $ questionUser tbl
    Left err -> return (Left err)

  let finalResult = do
        ast <- parseToSemantic parseResult
        st <- semanticResult
        vt <- valueResult
        generateOutput ast (fst st) vt filePath

  writeFinalResult filePath finalResult

-- Output Directory

createOutputDirectory :: FilePath -> IO ()
createOutputDirectory filePath = do
  createDirectoryIfMissing True $ dropExtension filePath

-- Parse Functions

writeParseResult :: String -> Either ParseError AST.Template-> IO ()
writeParseResult filePath parseResult = do
  writeToFile
    filePath
    "/AST.txt"
    (parseResultToString parseResult)

parseString :: String -> Either ParseError AST.Template
parseString = parse coderunnerParser ""

parseResultToString :: Either ParseError AST.Template -> String
parseResultToString parseResult = case parseResult of
  Left a -> show a
  Right b -> show b

-- Semantic Analysis Functions

writeSemanticResult :: String -> Either String (ST.SymbolTable, CG.ConstraintGraph) -> IO ()
writeSemanticResult filePath result = do
  let output = case result of
        Right (st, cg) -> (ST.showTable st, unpack (pShowNoColor cg) ++ "\n\n" ++ unpack (pShowNoColor (CG.configs cg)))
        Left err -> (err, err)
  writeToFile filePath "/ST.txt" (fst output)
  writeToFile filePath "/CG.txt" (snd output)



-- Final result
writeFinalResult :: String -> Either String String -> IO ()
writeFinalResult filePath result =
  let output = case result of
        Left err -> err
        Right res -> res
   in do
        writeToFile
          filePath
          "/Res.xml"
          output

-- Helper

parseToSemantic :: Either ParseError a -> Either String a
parseToSemantic (Left error) = Left "Error while Parsing"
parseToSemantic (Right a) = Right a

writeToFile :: String -> String -> String -> IO ()
writeToFile inputFilePath fileName output = do
  let path = takeDirectory inputFilePath </> takeBaseName inputFilePath ++ fileName
  outputHandle <- openFile path WriteMode
  hSetEncoding outputHandle utf8
  hPutStr outputHandle output
  hClose outputHandle
