module CoderunnerGenerator.Main where

import CoderunnerGenerator.ConfigGeneration (computeConfigurations)
import CoderunnerGenerator.Types.App (App)
import CoderunnerGenerator.Types.Globals
import Control.Monad (foldM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Reader (asks)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropExtension, takeBaseName, takeDirectory, (</>))
import System.IO
import Text.Pretty.Simple (pPrint)

main :: App s ()
main = do
  fileContent <- readTemplateFile
  createOutputDirectory

  parser <- asks getParser
  (parseResult, s) <- lift . except $ parser fileContent
  debugOutput parseResult

  configs <- computeConfigurations parseResult
  debugOutput configs

  generator <- asks getGenerator
  results <- lift . except $ generator configs s
  writeResults results

  liftIO $ putStrLn $ "Generated " ++ (show . length) results ++ " instances"

-- | Read content of the template file
readTemplateFile :: App s String
readTemplateFile = do
  filePath <- asks getTemplateFilePath
  inputHandle <- liftIO $ openFile filePath ReadMode
  liftIO $ hSetEncoding inputHandle utf8
  liftIO $ hGetContents inputHandle

-- | Write results to the output files
writeResults :: [String] -> App s ()
writeResults = foldM_ f 1
  where
    f :: Int -> String -> App s Int
    f acc s = writeToFile ("result" ++ show acc ++ ".xml") s >> return (acc + 1)

-- | Create directory for the output files
createOutputDirectory :: App s ()
createOutputDirectory = do
  filePath <- asks getTemplateFilePath
  liftIO $ createDirectoryIfMissing True $ dropExtension filePath

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