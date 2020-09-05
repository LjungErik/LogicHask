module Math.Logic.TruthTable
(
    TruthTable(expression, truthVars, columns),
    TruthValues(logicValues, result),
    generateTruthTable,
    isTautology,
    isContradiction
)
where
import Data.Map as M hiding (map) 
import Math.Logic.Parsing as P 
import Math.Logic.Evaluation as E

data TruthValues = TruthValues {
    logicValues :: [(String, Bool)],
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
                                        truth = TruthValues { logicValues = (t), result = result } in (truth:values)
generateTruthTable'' rpn (x:xs) t values = generateTruthTable'' rpn xs (t ++ [(x,True)]) (generateTruthTable'' rpn xs (t ++ [(x,False)]) values)

isTautology :: TruthTable -> Bool
isTautology t = and $ map result $ columns t

isContradiction :: TruthTable -> Bool
isContradiction t = and $ map (\x -> not $ result x) $ columns t
