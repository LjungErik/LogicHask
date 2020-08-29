module Math.PrintOut 
(
    outputTruthTable
) where

import Math.Logic as L

-- To beging with writting to console or writing to file

-- expected style:
-- ___p___|___q___|___p_∧_q___
--  TRUE  | TRUE  | TRUE   
--  TRUE  | FALSE | FALSE
--  FALSE | TRUE  | FALSE
--  FALSE | FALSE | FALSE

outputTruthTable :: L.TruthTable -> IO ()
outputTruthTable t = do
    outputHeader (L.truthVars t) (L.expression t)
    outputTruthValues (L.columns t)

outputHeader :: [String] -> String -> IO ()
outputHeader [] exp = putStrLn ("__" ++ exp ++ "__")
outputHeader (var:variables) exp = do
                        putStr ("___" ++ var ++ "___|")
                        outputHeader variables exp
 
outputTruthValues :: [L.TruthValues] -> IO ()
outputTruthValues [] = putStrLn ""
outputTruthValues (truth:values) = do
    printTruths (L.axiomsValues truth) (L.result truth)
    outputTruthValues values

printTruths :: [(String, Bool)] -> Bool -> IO ()
printTruths [] result = putStrLn (" " ++ (toBoolStr result)  ++ " ")
printTruths ((_, val):truths) result = do
                                    putStr (" " ++ (toBoolStr val) ++ " |")
                                    printTruths truths result

toBoolStr :: Bool -> String
toBoolStr True  = "TRUE "
toBoolStr False = "FALSE"
