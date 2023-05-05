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
