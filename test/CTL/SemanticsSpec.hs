module CTL.SemanticsSpec (spec) where

import CTL.Parser (parseCTL)
import CTL.Semantics (evaluateCTL)
import TS.Model
import TS.Parser (parseTS)
import Test.Hspec (Spec, describe, it, shouldBe)

vendingMachine :: IO (Either String TransitionSystem)
vendingMachine = do
  file <- readFile "examples/vending-machine.txt"
  return $ parseTS file

doEvaluate :: Either String TransitionSystem -> String -> Either String Bool
doEvaluate ts formula = do
  ts' <- ts
  f <- parseCTL formula
  return $ evaluateCTL ts' f

spec :: Spec
spec = describe "ctl model checking" $ do
  describe "the examples" $ do
    it "always eventually pay" $ do
      ts <- vendingMachine
      let formula = "A (F pay)"
      doEvaluate ts formula `shouldBe` Right True
    it "exists eventually soda" $ do
      ts <- vendingMachine
      let formula = "E (F soda)"
      doEvaluate ts formula `shouldBe` Right True
    it "exists always selection followed by soda" $ do
      ts <- vendingMachine
      let formula = "E (G (select -> A (X soda)))"
      doEvaluate ts formula `shouldBe` Right False
    it "always eventually soda" $ do
      ts <- vendingMachine
      let formula = "A (F soda)"
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
    it "evaluates an existential quantifier" $ do
      ts <- vendingMachine
      let formula = "E (F pay)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid existential quantifier" $ do
      ts <- vendingMachine
      let formula = "E (G beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates a universal quantifier" $ do
      ts <- vendingMachine
      let formula = "A (F pay)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid universal quantifier" $ do
      ts <- vendingMachine
      let formula = "A (F beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates a next" $ do
      ts <- vendingMachine
      let formula = "A (X select)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid next" $ do
      ts <- vendingMachine
      let formula = "E (X beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates an eventually" $ do
      ts <- vendingMachine
      let formula = "A (F select)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid eventually" $ do
      ts <- vendingMachine
      let formula = "E (F (soda && beer))"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates an always" $ do
      ts <- vendingMachine
      let formula = "A (G ((pay || select) || A (X pay)))"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid always" $ do
      ts <- vendingMachine
      let formula = "E (G beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "evaluates an until" $ do
      ts <- vendingMachine
      let formula = "A (pay U select)"
      doEvaluate ts formula `shouldBe` Right True
    it "evaluates an invalid until" $ do
      ts <- vendingMachine
      let formula = "E (pay U beer)"
      doEvaluate ts formula `shouldBe` Right False
