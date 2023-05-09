module LTL.SemanticsSpec (spec) where

import Data.Either (fromRight)
import LTL.Parser (parseLTL)
import LTL.Semantics (boundedPaths, evaluateLTL, trace)
import TS.Model
import TS.Parser (parseTS)
import Test.Hspec (Spec, describe, it, shouldBe)

vendingMachine :: IO TransitionSystem
vendingMachine = do
  file <- readFile "examples/vending-machine.txt"
  return $ fromRight (error "unwrap failed") (parseTS file)

doEvaluate :: TransitionSystem -> String -> Either String Bool
doEvaluate ts formula = do
  f <- parseLTL formula
  return $ evaluateLTL ts f 4

spec :: Spec
spec = describe "bounded ltl model checking" $ do
  describe "the internals" $ do
    it "computes bounded paths from initial states" $ do
      ts <- vendingMachine
      boundedPaths ts 4 `shouldBe` [[State "pay", State "select", State "beer", State "pay"], [State "pay", State "select", State "soda", State "pay"]]
    it "computes traces from paths" $ do
      ts <- vendingMachine
      let path = [State "pay", State "select", State "beer", State "pay"]
      trace ts path `shouldBe` [[AtomicProposition "pay"], [AtomicProposition "select"], [AtomicProposition "beer"], [AtomicProposition "pay"]]
  describe "the examples" $ do
    it "always eventually pay" $ do
      ts <- vendingMachine
      let formula = "(F pay)"
      doEvaluate ts formula `shouldBe` Right True
    it "exists eventually soda" $ do
      ts <- vendingMachine
      let formula = "(F soda)"
      doEvaluate ts formula `shouldBe` Right False
    it "exists always selection followed by soda" $ do
      ts <- vendingMachine
      let formula = "(G (select -> (X soda)))"
      doEvaluate ts formula `shouldBe` Right False
  describe "the algorithm" $ do
    it "evaluates a boolean literal" $ do
      ts <- vendingMachine
      let formula = "true"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates a proposition" $ do
      ts <- vendingMachine
      let formula = "pay"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid proposition" $ do
      ts <- vendingMachine
      let formula = "beer"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates a negation" $ do
      ts <- vendingMachine
      let formula = "!(beer)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid negation" $ do
      ts <- vendingMachine
      let formula = "!(pay)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates a conjunction" $ do
      ts <- vendingMachine
      let formula = "(pay && !(select))"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid conjunction" $ do
      ts <- vendingMachine
      let formula = "(pay && select)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates a disjunction" $ do
      ts <- vendingMachine
      let formula = "(pay || select)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid conjunction" $ do
      ts <- vendingMachine
      let formula = "(beer || select)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates an implication" $ do
      ts <- vendingMachine
      let formula = "(true -> pay)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid implication" $ do
      ts <- vendingMachine
      let formula = "(pay -> select)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates an equivalence" $ do
      ts <- vendingMachine
      let formula = "(pay <-> true)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid equivalence" $ do
      ts <- vendingMachine
      let formula = "(pay <-> select)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates a xor" $ do
      ts <- vendingMachine
      let formula = "(pay xor select)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid xor" $ do
      ts <- vendingMachine
      let formula = "(pay xor true)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates a next" $ do
      ts <- vendingMachine
      let formula = "(X select)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid next" $ do
      ts <- vendingMachine
      let formula = "(X beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates an eventually" $ do
      ts <- vendingMachine
      let formula = "(F select)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid eventually" $ do
      ts <- vendingMachine
      let formula = "(F beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates an always" $ do
      ts <- vendingMachine
      let formula = "(G ((pay || select) || (X pay)))"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid always" $ do
      ts <- vendingMachine
      let formula = "(G beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates an until" $ do
      ts <- vendingMachine
      let formula = "(pay U select)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid until" $ do
      ts <- vendingMachine
      let formula = "(pay U beer)"
      doEvaluate ts formula `shouldBe` Right False
