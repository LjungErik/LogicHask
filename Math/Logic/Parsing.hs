module Math.Logic.Parsing
(
    LogicExpression(logicRPN, expVars),
    parseTokens,
    parseLogicExpression
)
where

import Data.Char (isLetter, isSpace)
import Data.Set as S

data LogicExpression = LogicExpression {
    logicRPN :: [String],
    expVars :: [String]
} deriving (Show)

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
                                  vars = extractLogicVariables tokens in LogicExpression { logicRPN = rpn, expVars = (reverse (ensureNoDuplicates vars)) }

parseLogicAsRPN :: [String] -> [String]
parseLogicAsRPN tokens = parseLogicAsRPN' tokens [] [] 

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

extractLogicVariables' :: [String] -> [String] -> [String]
extractLogicVariables' [] vars = reverse vars
extractLogicVariables' (token:tokens) vars 
    | isOperator token || isParenthesis token = extractLogicVariables' tokens vars
    | otherwise = extractLogicVariables' tokens (token:vars)

extractLogicVariables :: [String] -> [String]
extractLogicVariables tokens = extractLogicVariables' tokens []

ensureNoDuplicates :: [String] -> [String]
ensureNoDuplicates vars = S.toDescList $ S.fromList vars

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