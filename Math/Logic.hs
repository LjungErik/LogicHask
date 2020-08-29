module Math.Logic 
(
    TruthValues(axiomsValues, result),
    TruthTable(expression, truthVars, columns),
    parseLogicAsRPN,
    parseTokens,
    extractLogicVariables,
    generateTruthTable
)
where

import Data.Char (isLetter, isSpace)
import Data.Map as M
import Data.Set as S

logicalOr = "or"
logicalAnd = "and"
logicalNot = "not"
logicalImplication = "=>"
logicalEquivalence = "<=>"

data LogicExpression = LogicExpression {
    logicRPN :: [String],
    variables :: [String]
} deriving (Show)

data TruthValues = TruthValues {
    axiomsValues :: [(String, Bool)],
    result :: Bool
} deriving (Show)

data TruthTable = TruthTable {
    expression :: String,
    truthVars :: [String],
    columns :: [TruthValues]
} deriving (Show)

generateTruthTable :: String -> TruthTable
generateTruthTable str = let exp = parseLogicExpression (parseTokens str) in TruthTable {
    expression = str,
    truthVars = variables exp,
    columns = generateTruthTable' exp
}

generateTruthTable' :: LogicExpression -> [TruthValues]
generateTruthTable' exp = generateTruthTable'' (logicRPN exp) (variables exp) [] []

generateTruthTable'' :: [String] -> [String] -> [(String, Bool)] ->  [TruthValues] -> [TruthValues]
generateTruthTable'' rpn [] t values = do
                                    let result = evaluateLogicRPN rpn (M.fromList t)
                                        truth = TruthValues { axiomsValues = t, result = result } in (truth:values)
generateTruthTable'' rpn (x:xs) t values = generateTruthTable'' rpn xs ((x,True):t) (generateTruthTable'' rpn xs ((x,False):t) values)

imply :: Bool -> Bool -> Bool
imply True x2 = x2
imply False x2 = True

evaluateLogicRPN' :: [String] -> (String -> Bool) -> [Bool] -> Bool
evaluateLogicRPN' [] _ (x:xs) = x
evaluateLogicRPN' ("not":tokens) truthTable (x:out) = evaluateLogicRPN' tokens (truthTable) ((not x):out)
evaluateLogicRPN' ("and":tokens) truthTable (x1:x2:out) = evaluateLogicRPN' tokens (truthTable) ((x2 && x1):out)
evaluateLogicRPN' ("or":tokens) truthTable (x1:x2:out) = evaluateLogicRPN' tokens (truthTable) ((x2 || x1):out)
evaluateLogicRPN' ("=>":tokens) truthTable (x1:x2:out) = evaluateLogicRPN' tokens (truthTable) ((imply x2 x1):out)
evaluateLogicRPN' ("<=>":tokens) truthTable (x1:x2:out) = evaluateLogicRPN' tokens (truthTable) ((x2 == x1):out)
evaluateLogicRPN' (token:tokens) truthTable out = evaluateLogicRPN' tokens (truthTable) ((truthTable token):out)

evaluateLogicRPN :: [String] -> M.Map String Bool -> Bool
evaluateLogicRPN rpn values = evaluateLogicRPN' rpn (\x -> getValue values x) []

getValue :: M.Map String Bool -> String -> Bool
getValue m token = case (M.lookup token m) of
    Just x -> x
    Nothing -> error "Failed to get value from lookup table"

ensureNoDuplicates :: [String] -> [String]
ensureNoDuplicates vars = S.toList $ S.fromList vars

parseTokens :: String -> [String]
parseTokens str = parseTokens' str [] []

parseTokens' :: String -> String -> [String] -> [String]
parseTokens' [] [] tokens = reverse tokens
parseTokens' [] token tokens = parseTokens' [] [] (token:tokens)
parseTokens' str token tokens
    | isOperator token || isParenthesis token = parseTokens' str [] (token:tokens)
    | isSpace c = case token of
        [] -> parseTokens' cs token tokens
        _ -> parseTokens' cs [] (token:tokens)
    | isOperatorChar c = case token of
        [] -> parseTokens' cs (token ++ [c]) tokens
        (x:xs) -> if (isOperatorChar x)
                  then parseTokens' cs (token ++ [c]) tokens
                  else parseTokens' cs [c] (token:tokens)
    | isValidChar c = parseTokens' cs (token ++ [c]) tokens
    | otherwise = error ("Invalid character used, no tokens contain character: '" ++ token ++ "'")
    where (c:cs) = str


parseLogicExpression :: [String] -> LogicExpression
parseLogicExpression tokens = let rpn = parseLogicAsRPN tokens
                                  vars = extractLogicVariables tokens in LogicExpression { logicRPN = rpn, variables = (ensureNoDuplicates vars) }

parseLogicAsRPN' :: [String] -> [String] -> [String] -> [String]
parseLogicAsRPN' [] [] rpn = reverse rpn
parseLogicAsRPN' [] (op:ops) rpn = parseLogicAsRPN' [] ops (op:rpn)
parseLogicAsRPN' (token:tokens) opStack rpn 
    | isOperator token = case opStack of
        [] -> parseLogicAsRPN' tokens (token:opStack) rpn
        (op:ops) -> if (operatorPrecedence token) <= (operatorPrecedence op)
                    then parseLogicAsRPN' (token:tokens) ops (op:rpn)
                    else parseLogicAsRPN' tokens (token:opStack) rpn
    | isLeftParenthesis token = parseLogicAsRPN' tokens (token:opStack) rpn
    | isRightParenthesis token = case opStack of
        ("(":ops) -> parseLogicAsRPN' tokens ops rpn
        (op:ops) -> parseLogicAsRPN' (token:tokens) ops (op:rpn)
        _ -> error "No begining parentheses '(' found, invalid expression"
    | otherwise = parseLogicAsRPN' tokens opStack (token:rpn) 

parseLogicAsRPN :: [String] -> [String]
parseLogicAsRPN tokens = parseLogicAsRPN' tokens [] [] 

extractLogicVariables' :: [String] -> [String] -> [String]
extractLogicVariables' [] vars = reverse vars
extractLogicVariables' (token:tokens) vars 
    | isOperator token || isParenthesis token = extractLogicVariables' tokens vars
    | otherwise = extractLogicVariables' tokens (token:vars)

extractLogicVariables :: [String] -> [String]
extractLogicVariables tokens = extractLogicVariables' tokens []

isLeftParenthesis :: String -> Bool
isLeftParenthesis "(" = True
isLeftParenthesis _ = False

isRightParenthesis :: String -> Bool
isRightParenthesis ")" = True
isRightParenthesis _ = False

isValidChar :: Char -> Bool
isValidChar '=' = True
isValidChar '>' = True
isValidChar '<' = True
isValidChar '(' = True
isValidChar ')' = True
isValidChar token
    | isLetter token = True
    | otherwise = False 

isOperatorChar :: Char -> Bool
isOperatorChar '=' = True
isOperatorChar '>' = True
isOperatorChar '<' = True
isOperatorChar '(' = True
isOperatorChar ')' = True
isOperatorChar _ = False

isOperator :: String -> Bool
isOperator "not" = True
isOperator "and" = True
isOperator "or" = True
isOperator "=>" = True
isOperator "<=>" = True
isOperator _ = False

isParenthesis :: String -> Bool
isParenthesis "(" = True
isParenthesis ")" = True
isParenthesis _ = False

operatorPrecedence :: String -> Int
operatorPrecedence "not" = 2
operatorPrecedence "and" = 1
operatorPrecedence "or" = 1
operatorPrecedence "=>" = 1
operatorPrecedence "<=>" = 1
operatorPrecedence _ = 0