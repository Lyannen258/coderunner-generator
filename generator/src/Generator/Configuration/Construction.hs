module Generator.Configuration.Construction where

import Generator.Configuration.Type
import Generator.ParameterName
import System.Random

empty :: IO Configuration
empty = do
  Configuration [] (GeneralInfo "" "" "" "") . randoms <$> getStdGen -- TODO add real general info

class ParameterValue v where
  addParameter :: ParameterName -> v -> [v] -> Configuration -> Configuration

instance ParameterValue String where
  addParameter pn sv avs c =
    c
      { parameters =
          parameters c ++ [Parameter pn (Single (SingleComponent sv avs))]
      }

instance ParameterValue [String] where
  addParameter pn sv avs c =
    c
      { parameters =
          parameters c ++ [Parameter pn (Multi (MultiComponent sv avs))]
      }