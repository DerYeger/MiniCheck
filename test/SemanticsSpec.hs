module SemanticsSpec (spec) where

import CTL.Parser (parseCTL)
import Semantics (evaluate)
import TS.Model
import TS.Parser (parseTS)
import Test.Hspec (Spec, describe, it, shouldBe)

vendingMachine :: IO TransitionSystem
vendingMachine = do
  file <- readFile "examples/vending-machine.txt"
  return $ parseTS file

spec :: Spec
spec = describe "from examples" $ do
  it "always eventually pay" $ do
    ts <- vendingMachine
    let formula = parseCTL "A (F pay)"
    evaluate ts formula `shouldBe` True
  it "exists eventually soda" $ do
    ts <- vendingMachine
    let formula = parseCTL "E (F soda)"
    evaluate ts formula `shouldBe` True
  it "exists always selection followed by soda" $ do
    ts <- vendingMachine
    let formula = parseCTL "E (G (select -> A (X soda)))"
    evaluate ts formula `shouldBe` True
  it "always eventually soda" $ do
    ts <- vendingMachine
    let formula = parseCTL "A (F soda)"
    evaluate ts formula `shouldBe` False
  describe "the basics" $ do
    it "evaluates a boolean literal" $ do
      ts <- vendingMachine
      let formula = parseCTL "true"
      evaluate ts formula `shouldBe` True
    it "evaluates a proposition" $ do
      ts <- vendingMachine
      let formula = parseCTL "pay"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid proposition" $ do
      ts <- vendingMachine
      let formula = parseCTL "beer"
      evaluate ts formula `shouldBe` False
    it "evaluates a negation" $ do
      ts <- vendingMachine
      let formula = parseCTL "!(beer)"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid negation" $ do
      ts <- vendingMachine
      let formula = parseCTL "!(pay)"
      evaluate ts formula `shouldBe` False
    it "evaluates a conjunction" $ do
      ts <- vendingMachine
      let formula = parseCTL "(pay && !(select))"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid conjunction" $ do
      ts <- vendingMachine
      let formula = parseCTL "(pay && select)"
      evaluate ts formula `shouldBe` False
    it "evaluates a disjunction" $ do
      ts <- vendingMachine
      let formula = parseCTL "(pay || select)"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid conjunction" $ do
      ts <- vendingMachine
      let formula = parseCTL "(beer || select)"
      evaluate ts formula `shouldBe` False
    it "evaluates an implication" $ do
      ts <- vendingMachine
      let formula = parseCTL "(true -> pay)"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid implication" $ do
      ts <- vendingMachine
      let formula = parseCTL "(pay -> select)"
      evaluate ts formula `shouldBe` False
    it "evaluates an equivalence" $ do
      ts <- vendingMachine
      let formula = parseCTL "(pay <-> true)"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid equivalence" $ do
      ts <- vendingMachine
      let formula = parseCTL "(pay <-> select)"
      evaluate ts formula `shouldBe` False
    it "evaluates a xor" $ do
      ts <- vendingMachine
      let formula = parseCTL "(pay xor select)"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid xor" $ do
      ts <- vendingMachine
      let formula = parseCTL "(pay xor true)"
      evaluate ts formula `shouldBe` False
    it "evaluates an existential quantifier" $ do
      ts <- vendingMachine
      let formula = parseCTL "E (F pay)"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid existential quantifier" $ do
      ts <- vendingMachine
      let formula = parseCTL "E (G beer)"
      evaluate ts formula `shouldBe` False
    it "evaluates a universal quantifier" $ do
      ts <- vendingMachine
      let formula = parseCTL "A (F pay)"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid universal quantifier" $ do
      ts <- vendingMachine
      let formula = parseCTL "A (F beer)"
      evaluate ts formula `shouldBe` False
    it "evaluates a next" $ do
      ts <- vendingMachine
      let formula = parseCTL "A (X select)"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid next" $ do
      ts <- vendingMachine
      let formula = parseCTL "E (X beer)"
      evaluate ts formula `shouldBe` False
    it "evaluates an eventually" $ do
      ts <- vendingMachine
      let formula = parseCTL "A (F select)"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid eventually" $ do
      ts <- vendingMachine
      let formula = parseCTL "E (G beer)"
      evaluate ts formula `shouldBe` False
    it "evaluates an always" $ do
      ts <- vendingMachine
      let formula = parseCTL "A (F select)"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid always" $ do
      ts <- vendingMachine
      let formula = parseCTL "E (G beer)"
      evaluate ts formula `shouldBe` False
    it "evaluates an until" $ do
      ts <- vendingMachine
      let formula = parseCTL "A (pay U select)"
      evaluate ts formula `shouldBe` True
    it "evaluates an invalid until" $ do
      ts <- vendingMachine
      let formula = parseCTL "E (pay U beer)"
      evaluate ts formula `shouldBe` False
