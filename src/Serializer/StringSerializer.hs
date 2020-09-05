module Serializer.StringSerializer 
(
    serializeTruthTable
) where

import Math.Logic.TruthTable as T

-- Generate string representation of truth table
serializeTruthTable :: T.TruthTable -> String
serializeTruthTable t = do
    let headers = createExpressionHeader ((T.truthVars t) ++ [T.expression t])
        rows = createTruthValueRows (T.columns t)
    unlines (headers:rows)

createExpressionHeader :: [String] -> String
createExpressionHeader expressions = concat (map formatHeaderColumn expressions)

createTruthValueRows :: [T.TruthValues] -> [String]
createTruthValueRows values = map createTruthValueRow values

createTruthValueRow :: T.TruthValues -> String
createTruthValueRow tv = concat $ (map formatRowColumn (T.logicValues tv)) ++ [formatRowColumn (" ", T.result tv)]

formatHeaderColumn :: String -> String
formatHeaderColumn s = appendSpaces (clearLength s) s

formatRowColumn :: (String, Bool) -> String
formatRowColumn (s,b) = appendSpaces (clearLength s) (boolString b)

clearLength :: String -> Int
clearLength s = max 7 ((length s)+2)

appendSpaces :: Int -> String -> String
appendSpaces l s = concat (s:(replicate (l-(length s)) " "))

boolString :: Bool -> String
boolString True = "TRUE"
boolString False = "FALSE"