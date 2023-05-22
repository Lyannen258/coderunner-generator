module Generator.Test where

{- simpleParseResultWithoutConstraints :: Either String ParseResult
simpleParseResultWithoutConstraints = do
  pr <- addParameter PR.empty (PR.makeParam (mkParameterName "p1") [Final "v1", Final "v2", Final "v3"])
  pr' <- addParameter pr (PR.makeParam (mkParameterName "p2") [Final "v1", Final "v2"])
  addParameter pr' (PR.makeParam (mkParameterName "p3")[Final "v1", Final "v2", Final "v3"])

simpleParseResult :: Either String ParseResult
simpleParseResult = do
  pr <- simpleParseResultWithoutConstraints
  addConstraint
    pr
    (mkParameterName "p1", PR.makeValue $ Final "v1")
    (mkParameterName "p2", PR.makeValue $ Final "v1") -}