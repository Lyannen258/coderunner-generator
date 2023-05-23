module Generator.Main (main) where

import Control.Monad (unless, when)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Generator.App (App)
import Generator.Configuration.FromParseResult (computeConfigurations, computeMaxAmount)
import Generator.Globals
import Generator.Helper (printLn)
import Generator.ParseResult (ParseResult)
import Generator.ToParseResult (ToParseResult (toParseResult))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropExtension, takeBaseName, takeDirectory, (</>))
import System.IO
import Text.Pretty.Simple (pPrint)

main :: (Show r, ToParseResult r) => App r u ()
main = do
  fileContent <- readTemplateFile
  createOutputDirectory

  parser <- asks getParser
  (ret, u) <- liftEither $ parser fileContent
  debugOutput ret
  parseResult <- liftEither $ toParseResult ret
  debugOutput parseResult

  maxFlagActive <- asks getMaxConfigurations
  if maxFlagActive
    then mainMaxAmount parseResult
    else mainConfigurations parseResult u

mainMaxAmount :: ParseResult -> App r u ()
mainMaxAmount pr = do
  maxAmount <- computeMaxAmount pr
  printMaxAmount maxAmount
  return ()

printMaxAmount :: Int -> App r u ()
printMaxAmount m = do
  printLn output
  where
    output = "Maximal amount of configurations: " ++ show m

mainConfigurations :: ParseResult -> u -> App r u ()
mainConfigurations parseResult u = do
  configs <- computeConfigurations parseResult

  generator <- asks getGenerator
  results <- liftEither $ generator configs u
  writeResult results

  printLn $ "Generated " ++ (show . length) configs ++ " variants"

-- | Read content of the template file
readTemplateFile :: App r u String
readTemplateFile = do
  filePath <- asks getTemplateFilePath
  inputHandle <- liftIO $ openFile filePath ReadMode
  liftIO $ hSetEncoding inputHandle utf8
  liftIO $ hGetContents inputHandle

-- | Write single result
writeResult :: String -> App r u ()
writeResult = writeToFile "result.xml"

-- | Create directory for the output files
createOutputDirectory :: App r u ()
createOutputDirectory = do
  maxFlagActive <- asks getMaxConfigurations
  filePath <- asks getTemplateFilePath
  unless maxFlagActive $ liftIO $ createDirectoryIfMissing True $ dropExtension filePath

-- | Write content to a file in the output directory
writeToFile :: String -> String -> App r u ()
writeToFile fileName output = do
  inputFilePath <- asks getTemplateFilePath
  let path = takeDirectory inputFilePath </> takeBaseName inputFilePath </> fileName
  outputHandle <- liftIO $ openFile path WriteMode
  liftIO $ hSetEncoding outputHandle utf8
  liftIO $ hPutStr outputHandle output
  liftIO $ hClose outputHandle

debugOutput :: Show a => a -> App r u ()
debugOutput a = do
  dbg <- asks getDebugOutputFlag
  liftIO $ when dbg (pPrint a)