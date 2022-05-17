module CoderunnerGenerator.Test where

import CoderunnerGenerator.ConfigGeneration
import CoderunnerGenerator.Types.ParseResult as PR
import Control.Monad.Trans.Reader
import qualified Data.Maybe
import Test.HUnit
import Text.Megaparsec
import Text.Pretty.Simple (pPrint)

simpleParseResultWithoutConstraints :: Either String ParseResult
simpleParseResultWithoutConstraints = do
  pr <- addParameter PR.empty (PR.singleParam "p1" [Final "v1", Final "v2", Final "v3"])
  pr' <- addParameter pr (PR.singleParam "p2" [Final "v1", Final "v2"])
  addParameter pr' (PR.singleParam "p3" [Final "v1", Final "v2", Final "v3"])

simpleParseResult :: Either String ParseResult
simpleParseResult = do
  pr <- simpleParseResultWithoutConstraints
  addConstraint
    pr
    ("p1", PR.singleValue $ Final "v1")
    ("p2", PR.singleValue $ Final "v1")

{- combinationsOfSimpleParseResult :: Test
combinationsOfSimpleParseResult = TestCase t
  where
    t :: IO ()
    t = do
      let combs = allCombinations simpleParseResultWithoutConstraints
      return () -}