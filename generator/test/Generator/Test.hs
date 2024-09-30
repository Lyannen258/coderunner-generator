module Generator.Test where

import Generator.Helper
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck (withMaxSuccess 10000 prop_fill)

prop_fill :: ASCIIString -> Bool
prop_fill ascii =
  let str = getASCIIString ascii
   in length (fillToTwenty str) >= 20