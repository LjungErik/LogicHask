module Main where

import Math.Logic.TruthTable as T

main :: IO ()
main = do
    let truthTable = T.generateTruthTable "(not (p and not q))"
    T.outputTruthTable truthTable