module Math.Logic.TruthTable
(
    TruthTable(expression, truthVars, columns),
    generateTruthTable,
    outputTruthTable
)
where
import Data.Map as M
import Math.Logic.Parsing as P
import Math.Logic.Evaluation as E

data TruthValues = TruthValues {
    axiomsValues :: [(String, Bool)],
    result :: Bool
} deriving (Show)

data TruthTable = TruthTable {
    expression :: String,
    truthVars :: [String],
    columns :: [TruthValues]
} deriving (Show)

-- Generating a truth table based in input logic statement
generateTruthTable :: String -> TruthTable
generateTruthTable str = let exp = P.parseLogicExpression (P.parseTokens str) in TruthTable {
    expression = str,
    truthVars = P.expVars exp,
    columns = generateTruthTable' exp
}

generateTruthTable' :: P.LogicExpression -> [TruthValues]
generateTruthTable' exp = generateTruthTable'' (P.logicRPN exp) (P.expVars exp) [] []

generateTruthTable'' :: [String] -> [String] -> [(String, Bool)] ->  [TruthValues] -> [TruthValues]
generateTruthTable'' rpn [] t values = do
                                    let result = E.evaluateLogicRPN rpn (M.fromList t)
                                        truth = TruthValues { axiomsValues = (t), result = result } in (truth:values)
generateTruthTable'' rpn (x:xs) t values = generateTruthTable'' rpn xs (t ++ [(x,True)]) (generateTruthTable'' rpn xs (t ++ [(x,False)]) values)

-- Output a TruthTable to console

outputTruthTable :: TruthTable -> IO ()
outputTruthTable t = do
    outputHeader (truthVars t) (expression t)
    outputTruthValues (columns t)

outputHeader :: [String] -> String -> IO ()
outputHeader [] exp = putStrLn ("__" ++ exp ++ "__")
outputHeader (var:variables) exp = do
                        putStr ("___" ++ var ++ "___|")
                        outputHeader variables exp
 
outputTruthValues :: [TruthValues] -> IO ()
outputTruthValues [] = putStrLn ""
outputTruthValues (truth:values) = do
    printTruths (axiomsValues truth) (result truth)
    outputTruthValues values

printTruths :: [(String, Bool)] -> Bool -> IO ()
printTruths [] result = putStrLn (" " ++ (toBoolStr result)  ++ " ")
printTruths ((_, val):truths) result = do
                                    putStr (" " ++ (toBoolStr val) ++ " |")
                                    printTruths truths result

toBoolStr :: Bool -> String
toBoolStr True  = "TRUE "
toBoolStr False = "FALSE"

