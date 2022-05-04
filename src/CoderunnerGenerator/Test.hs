module CoderunnerGenerator.Test where

import CoderunnerGenerator.ConfigGeneration
import CoderunnerGenerator.Types.ParseResult as PR
import Control.Monad.Trans.Reader
import Test.HUnit
import Text.Megaparsec
import Text.Pretty.Simple (pPrint)
import qualified Data.Maybe

simpleParseResultWithoutConstraints :: ParseResult
simpleParseResultWithoutConstraints =
  let pr = addValues PR.empty "p1" [Final "v1", Final "v2", Final "v3"]
      pr' = addValues pr "p2" [Final "v1", Final "v2"]
      pr'' = addValues pr' "p3" [Final "v1", Final "v2", Final "v3"]
   in pr''

simpleParseResult :: ParseResult
simpleParseResult = Data.Maybe.fromMaybe undefined maybePR
  where
    maybePR = addConstraint
      simpleParseResultWithoutConstraints
      ("p1", Final "v1")
      ("p2", Final "v1")

{- combinationsOfSimpleParseResult :: Test
combinationsOfSimpleParseResult = TestCase t
  where
    t :: IO ()
    t = do
      let combs = allCombinations simpleParseResultWithoutConstraints
      return () -}