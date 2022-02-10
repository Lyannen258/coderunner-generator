module CoderunnerGenerator.ConfigGeneration where

import CoderunnerGenerator.Interaction (InteractionResult)
import CoderunnerGenerator.Types.ConstraintGraph (ConstraintGraph)
import qualified CoderunnerGenerator.Types.ConstraintGraph as CG
import CoderunnerGenerator.Types.SymbolTable (SymbolTable)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Lens.Micro.Extras (view)
import System.Random (RandomGen, getStdGen, uniformR)

generateConfigs :: Int -> (SymbolTable, ConstraintGraph) -> IO [Map String [String]]
generateConfigs a (st, cg) = do
  let configs = CG.configs cg
  randomNumbers <- getRandomNumbers a (length configs)
  let valueTables = map (generateValueTable configs) randomNumbers
  return valueTables

generateValueTable :: Set (Set CG.Value) -> Int -> Map String [String]
generateValueTable configs rand =
  let randomConfig = S.elemAt rand configs
   in S.foldr folder M.empty randomConfig
  where
    folder v acc =
      M.insert
        (view CG.parameter v)
        (view CG.value v)
        acc

getRandomNumbers :: Int -> Int -> IO [Int]
getRandomNumbers amountNumbers amountConfigs = do
  stdGen <- getStdGen
  let nDistinct = getNDistinct amountNumbers amountConfigs stdGen
  return nDistinct

getNDistinct :: RandomGen g => Int -> Int -> g -> [Int]
getNDistinct amountNumbers amountConfigs = fillToN []
  where
    fillToN acc std
      | length (nub acc) >= min amountNumbers amountConfigs = nub acc
      | otherwise =
        let (rand, newGen) = uniformR (0, amountConfigs - 1) std
         in fillToN (rand : acc) newGen