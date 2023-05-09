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
spec = describe "from examples" $ do
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
    doEvaluate ts formula `shouldBe` Right True
  it "always eventually soda" $ do
    ts <- vendingMachine
    let formula = "A (F soda)"
    doEvaluate ts formula `shouldBe` Right False
  describe "the basics" $ do
    it "doEvaluates a boolean literal" $ do
      ts <- vendingMachine
      let formula = "true"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates a proposition" $ do
      ts <- vendingMachine
      let formula = "pay"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid proposition" $ do
      ts <- vendingMachine
      let formula = "beer"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates a negation" $ do
      ts <- vendingMachine
      let formula = "!(beer)"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid negation" $ do
      ts <- vendingMachine
      let formula = "!(pay)"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates a conjunction" $ do
      ts <- vendingMachine
      let formula = "(pay && !(select))"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid conjunction" $ do
      ts <- vendingMachine
      let formula = "(pay && select)"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates a disjunction" $ do
      ts <- vendingMachine
      let formula = "(pay || select)"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid conjunction" $ do
      ts <- vendingMachine
      let formula = "(beer || select)"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates an implication" $ do
      ts <- vendingMachine
      let formula = "(true -> pay)"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid implication" $ do
      ts <- vendingMachine
      let formula = "(pay -> select)"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates an equivalence" $ do
      ts <- vendingMachine
      let formula = "(pay <-> true)"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid equivalence" $ do
      ts <- vendingMachine
      let formula = "(pay <-> select)"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates a xor" $ do
      ts <- vendingMachine
      let formula = "(pay xor select)"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid xor" $ do
      ts <- vendingMachine
      let formula = "(pay xor true)"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates an existential quantifier" $ do
      ts <- vendingMachine
      let formula = "E (F pay)"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid existential quantifier" $ do
      ts <- vendingMachine
      let formula = "E (G beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates a universal quantifier" $ do
      ts <- vendingMachine
      let formula = "A (F pay)"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid universal quantifier" $ do
      ts <- vendingMachine
      let formula = "A (F beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates a next" $ do
      ts <- vendingMachine
      let formula = "A (X select)"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid next" $ do
      ts <- vendingMachine
      let formula = "E (X beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates an eventually" $ do
      ts <- vendingMachine
      let formula = "A (F select)"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid eventually" $ do
      ts <- vendingMachine
      let formula = "E (G beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates an always" $ do
      ts <- vendingMachine
      let formula = "A (F select)"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid always" $ do
      ts <- vendingMachine
      let formula = "E (G beer)"
      doEvaluate ts formula `shouldBe` Right False
    it "doEvaluates an until" $ do
      ts <- vendingMachine
      let formula = "A (pay U select)"
      doEvaluate ts formula `shouldBe` Right True
    it "doEvaluates an invalid until" $ do
      ts <- vendingMachine
      let formula = "E (pay U beer)"
      doEvaluate ts formula `shouldBe` Right False
