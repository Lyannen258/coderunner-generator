module CoderunnerGenerator.Main where

import CoderunnerGenerator.CmdArgs (Args)
import qualified CoderunnerGenerator.CmdArgs as CmdArgs
import CoderunnerGenerator.ConfigGeneration (computeConfigurations)
import CoderunnerGenerator.Helper
import CoderunnerGenerator.Types.App (App)
import CoderunnerGenerator.Types.Globals
import Control.Exception (SomeException, try)
import Control.Monad (foldM_, sequence, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Map (Map)
import Data.Text.Lazy (unpack)
import Data.Tree (drawTree)
import Data.Void (Void)
import System.Directory
import System.Environment (getArgs)
import System.FilePath (dropExtension, takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)
import Text.Pretty.Simple (pPrint, pShowNoColor)

main :: App s ()
main = do
  fileContent <- readTemplateFile
  createOutputDirectory

  parser <- asks getParser
  (parseResult, s) <- lift . except $ parser fileContent

  configs <- computeConfigurations parseResult
  pPrint configs

  generator <- asks getGenerator
  let results = generator configs s
  pPrint results

-- | Read content of the template file
readTemplateFile :: App s String
readTemplateFile = do
  filePath <- asks getTemplateFilePath
  inputHandle <- liftIO $ openFile filePath ReadMode
  liftIO $ hSetEncoding inputHandle utf8
  liftIO $ hGetContents inputHandle

-- | Create directory for the output files
createOutputDirectory :: App s ()
createOutputDirectory = do
  filePath <- asks getTemplateFilePath
  liftIO $ createDirectoryIfMissing True $ dropExtension filePath

{-

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
getFinalResult ::
  Either (ParseErrorBundle String Void) AST.Template ->
  Either String (ST.SymbolTable, b) ->
  Either String [ValueTable] ->
  String ->
  Either String [String]
getFinalResult parseResult semanticResult valueResult name = do
  ast <- parseToSemantic parseResult
  st <- semanticResult
  vt <- valueResult
  sequence $ generateOutputs ast (fst st) name vt

-- * Parse Functions

writeParseResult :: String -> Either (ParseErrorBundle String Void) AST.Template -> IO ()
writeParseResult filePath parseResult = do
  writeToFile
    filePath
    "/AST.txt"
    (parseResultToString parseResult)

parseString :: String -> Either (ParseErrorBundle String Void) AST.Template
parseString = parse coderunnerParser ""

parseResultToString :: Either (ParseErrorBundle String Void) AST.Template -> String
parseResultToString parseResult = case parseResult of
  Left a -> errorBundlePretty a
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

parseToSemantic :: Either (ParseErrorBundle String Void) a -> Either String a
parseToSemantic (Left error) = Left "Error while Parsing"
parseToSemantic (Right a) = Right a

writeToFile :: String -> String -> String -> IO ()
writeToFile inputFilePath fileName output = do
  let path = takeDirectory inputFilePath </> takeBaseName inputFilePath ++ fileName
  outputHandle <- openFile path WriteMode
  hSetEncoding outputHandle utf8
  hPutStr outputHandle output
  hClose outputHandle

-- * Error handling

 -}