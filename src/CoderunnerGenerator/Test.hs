module CoderunnerGenerator.Test where

import Test.HUnit
import Control.Monad.Trans.Reader
import CoderunnerGenerator.Parser
import Text.Megaparsec
import Text.Pretty.Simple (pPrint)

{- import CoderunnerGenerator.SemanticAnalyzer (semanticAnalysis)

firstExample :: Test
firstExample =
  TestCase
    ( do
        i <- readFile "example-files/01_copy.tmpl"
        let res = runReader (runParserT coderunnerParser "" i) ["Task", "Solution", "PreAllocation", "Tests"]
        case res of
            Left err -> assertString $ errorBundlePretty err
            Right x -> do {
                pPrint x;
                semanticAnalysis x;
              }


    ) -}


import Text.Megaparsec.Char
import Data.Void
entlein :: IO ()
entlein = parseTest (printChar :: Parsec Void String Char) "🦆"