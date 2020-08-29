module Main where

import Math.Logic as L
import Math.PrintOut as P

main :: IO ()
main = do
    let truthTable = L.generateTruthTable "not (p and not q)"
    P.outputTruthTable truthTable