module Main where

import CoderunnerGenerator.CmdArgs (Args)
import qualified CoderunnerGenerator.CmdArgs as CmdArgs
import CoderunnerGenerator.ConfigGeneration (generateConfigs)
import CoderunnerGenerator.Generator
import CoderunnerGenerator.Helper
import CoderunnerGenerator.Interaction
import CoderunnerGenerator.Parser
import qualified CoderunnerGenerator.SemanticAnalyzer as SA
import qualified CoderunnerGenerator.Types.AbstractSyntaxTree as AST
import qualified CoderunnerGenerator.Types.ConstraintGraph as CG
import qualified CoderunnerGenerator.Types.SymbolTable as ST
import Control.Monad (sequence, foldM_)
import Control.Monad.Trans.Except (runExceptT)
import Data.Map (Map)
import Data.Text.Lazy (unpack)
import Data.Tree (drawTree)
import System.Directory
import System.Environment (getArgs)
import System.FilePath (dropExtension, takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO
import Text.Parsec
import Text.Pretty.Simple (pPrint, pShowNoColor)

main :: IO ()
main = do
  args <- CmdArgs.executeParser
  analyzeFile args

analyzeFile :: Args -> IO ()
analyzeFile args = do
  let filePath = CmdArgs.templateFile args
  let amount = CmdArgs.amount args
  let name = takeBaseName filePath

  fileContent <- readFileContent filePath

  createOutputDirectory filePath

  let parseResult = parseString fileContent
  writeParseResult filePath parseResult

  let semanticResult = parseToSemantic parseResult >>= SA.semanticAnalysis
  writeSemanticResult filePath semanticResult

  valueResult <- case semanticResult of
    Right sr -> getValueTables amount sr
    Left err -> return $ Left err

  let finalResult = getFinalResult parseResult semanticResult valueResult name

  writeFinalResult filePath finalResult

-- | Read content of the template file
readFileContent :: String -> IO String
readFileContent filePath = do
  inputHandle <- openFile filePath ReadMode
  hSetEncoding inputHandle utf8
  hGetContents inputHandle

-- | Create directory for the output files
createOutputDirectory :: FilePath -> IO ()
createOutputDirectory filePath = do
  createDirectoryIfMissing True $ dropExtension filePath

-- | Get the value result by asking the user for values or automatically generating configurations.
getValueTables :: Maybe Int -> (ST.SymbolTable, CG.ConstraintGraph) -> IO (Either String [Map String [String]])
getValueTables (Just a) sr = Right <$> generateConfigs a sr
getValueTables Nothing sr = do
  ioEitherInteractionRes <- runExceptT $ questionUser sr
  case ioEitherInteractionRes of
    Left s -> return $ Left s
    Right ir ->
      case ir of
        ValueResult m -> return $ Right [m]
        AmountResult n -> Right <$> generateConfigs n sr

-- | Generate the coderunner exercises. Uses all intermediate results
-- (Parser, SemanticAnalyzer, Interaction/ValueTable)
getFinalResult :: Either ParseError AST.Template
  -> Either String (ST.SymbolTable, b)
  -> Either String [ValueTable]
  -> String
  -> Either String [String]
getFinalResult parseResult semanticResult valueResult name = do
  ast <- parseToSemantic parseResult
  st <- semanticResult
  vt <- valueResult
  sequence $ generateOutputs ast (fst st) name vt

-- * Parse Functions

writeParseResult :: String -> Either ParseError AST.Template -> IO ()
writeParseResult filePath parseResult = do
  writeToFile
    filePath
    "/AST.txt"
    (parseResultToString parseResult)

parseString :: String -> Either ParseError AST.Template
parseString = parse coderunnerParser ""

parseResultToString :: Either ParseError AST.Template -> String
parseResultToString parseResult = case parseResult of
  Left a -> unpack $ pShowNoColor a
  Right b -> unpack $ pShowNoColor b

-- * Semantic Analysis Functions

writeSemanticResult :: String -> Either String (ST.SymbolTable, CG.ConstraintGraph) -> IO ()
writeSemanticResult filePath result = do
  let output = case result of
        Right (st, cg) -> (unpack (pShowNoColor st), unpack (pShowNoColor cg) ++ "\n\n" ++ unpack (pShowNoColor (CG.configs cg)))
        Left err -> (err, err)
  writeToFile filePath "/ST.txt" (fst output)
  writeToFile filePath "/CG.txt" (snd output)

-- * Final result

writeFinalResult :: String -> Either String [String] -> IO ()
writeFinalResult filePath (Right results) = foldM_ writeSingleResult 1 (reverse results) -- reverse because foldM_ works in the wrong direction
  where
    writeSingleResult counter result = do
            writeToFile
              filePath
              ("/Res_" ++ show counter ++ ".xml")
              result 
            return (counter + 1)
writeFinalResult filePath (Left error) = writeToFile filePath "/Res.xml" error

-- * Helper

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
