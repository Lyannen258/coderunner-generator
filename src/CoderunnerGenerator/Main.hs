module CoderunnerGenerator.Main (main) where

import CoderunnerGenerator.ConfigGeneration (computeConfigurations, computeMaxAmount)
import CoderunnerGenerator.Types.App (App)
import CoderunnerGenerator.Types.Globals
import CoderunnerGenerator.Types.ParseResult (ParseResult)
import Control.Monad (foldM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Reader (asks)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropExtension, takeBaseName, takeDirectory, (</>))
import System.IO
import Text.Pretty.Simple (pPrint)
import CoderunnerGenerator.Helper (printLn)

main :: App s ()
main = do
  fileContent <- readTemplateFile
  createOutputDirectory

  parser <- asks getParser
  (parseResult, s) <- lift . except $ parser fileContent
  debugOutput parseResult

  maxFlagActive <- asks getMaxConfigurations
  if maxFlagActive
    then mainMaxAmount parseResult
    else mainConfigurations parseResult s

mainMaxAmount :: ParseResult -> App s ()
mainMaxAmount pr = do
  maxAmount <- computeMaxAmount pr
  printMaxAmount maxAmount
  return ()

printMaxAmount :: Int -> App s ()
printMaxAmount m = do
  printLn output
  where 
    output = "Maximal amount of configurations: " ++ show m

mainConfigurations :: ParseResult -> s -> App s ()
mainConfigurations parseResult s = do
  configs <- computeConfigurations parseResult
  debugOutput configs

  generator <- asks getGenerator
  results <- lift . except $ generator configs s
  writeResult results

  printLn $ "Generated " ++ (show . length) configs ++ " variants"

-- | Read content of the template file
readTemplateFile :: App s String
readTemplateFile = do
  filePath <- asks getTemplateFilePath
  inputHandle <- liftIO $ openFile filePath ReadMode
  liftIO $ hSetEncoding inputHandle utf8
  liftIO $ hGetContents inputHandle

-- | Write single result
writeResult :: String -> App s ()
writeResult = writeToFile "result.xml"

-- | Write results to the output files
writeResults :: [String] -> App s ()
writeResults = foldM_ f 1
  where
    f :: Int -> String -> App s Int
    f acc s = writeToFile ("result" ++ show acc ++ ".xml") s >> return (acc + 1)

-- | Create directory for the output files
createOutputDirectory :: App s ()
createOutputDirectory = do
  maxFlagActive <- asks getMaxConfigurations
  filePath <- asks getTemplateFilePath
  when maxFlagActive $ liftIO $ createDirectoryIfMissing True $ dropExtension filePath

-- | Write content to a file in the output directory
writeToFile :: String -> String -> App s ()
writeToFile fileName output = do
  inputFilePath <- asks getTemplateFilePath
  let path = takeDirectory inputFilePath </> takeBaseName inputFilePath </> fileName
  outputHandle <- liftIO $ openFile path WriteMode
  liftIO $ hSetEncoding outputHandle utf8
  liftIO $ hPutStr outputHandle output
  liftIO $ hClose outputHandle

debugOutput :: Show a => a -> App s ()
debugOutput a = do
  dbg <- asks getDebugOutputFlag
  liftIO $ when dbg (pPrint a)