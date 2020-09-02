module Math.Logic.EvaluationSpec (spec) where

import Math.Logic.Evaluation (evaluateLogicRPN)

import Test.Hspec
import Data.Map as M

spec :: Spec
spec = do
    describe "evaluateLogicRPN" $ do
        it "returns False for expression 'not p' where p is True" $ do
            (evaluateLogicRPN ["p", "not"] $ M.fromList [("p", True)]) `shouldBe` False
        it "returns True for expression 'not p' where p is False" $ do
            (evaluateLogicRPN ["p", "not"] $ M.fromList [("p", False)]) `shouldBe` True
        
        it "return False for expression 'p and q' where p is False and q is False" $ do
            (evaluateLogicRPN ["p", "q", "and"] $ M.fromList [("p", False),("q", False)]) `shouldBe` False
        it "return False for expression 'p and q' where p is False and q is True" $ do
            (evaluateLogicRPN ["p", "q", "and"] $ M.fromList [("p", False),("q", True)]) `shouldBe` False
        it "return False for expression 'p and q' where p is True and q is False" $ do
            (evaluateLogicRPN ["p", "q", "and"] $ M.fromList [("p", True),("q", False)]) `shouldBe` False
        it "return True for expression 'p and q' where p is True and q is True" $ do
            (evaluateLogicRPN ["p", "q", "and"] $ M.fromList [("p", True),("q", True)]) `shouldBe` True
            