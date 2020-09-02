module Math.Logic.Evaluation 
(
    evaluateLogicRPN
) where
import Data.Map as M

evaluateLogicRPN :: [String] -> M.Map String Bool -> Bool
evaluateLogicRPN rpn values = evaluateLogicRPN' rpn (\x -> getValue values x) []

evaluateLogicRPN' :: [String] -> (String -> Bool) -> [Bool] -> Bool
evaluateLogicRPN' [] _ (x:xs) = x
evaluateLogicRPN' ("not":tokens) truthTable (x:out) = evaluateLogicRPN' tokens (truthTable) ((not x):out)
evaluateLogicRPN' ("and":tokens) truthTable (x1:x2:out) = evaluateLogicRPN' tokens (truthTable) ((x2 && x1):out)
evaluateLogicRPN' ("or":tokens) truthTable (x1:x2:out) = evaluateLogicRPN' tokens (truthTable) ((x2 || x1):out)
evaluateLogicRPN' ("=>":tokens) truthTable (x1:x2:out) = evaluateLogicRPN' tokens (truthTable) ((imply x2 x1):out)
evaluateLogicRPN' ("<=>":tokens) truthTable (x1:x2:out) = evaluateLogicRPN' tokens (truthTable) ((x2 == x1):out)
evaluateLogicRPN' (token:tokens) truthTable out = evaluateLogicRPN' tokens (truthTable) ((truthTable token):out)

getValue :: M.Map String Bool -> String -> Bool
getValue m token = case (M.lookup token m) of
    Just x -> x
    Nothing -> error "Failed to get value from lookup table"

imply :: Bool -> Bool -> Bool
imply True x = x
imply False x = True