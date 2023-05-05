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
spec = describe "the semantics" $ do
  it "always eventually pay" $ do
    ts <- vendingMachine
    let formula = parseCTL "A (F pay)"
    let start = State "pay" -- todo randomize
    evaluate ts formula start `shouldBe` True
  it "exists eventually soda" $ do
    ts <- vendingMachine
    let formula = parseCTL "E (F soda)"
    let start = State "pay" -- todo randomize
    evaluate ts formula start `shouldBe` True
  it "exists always selection followed by soda" $ do
    ts <- vendingMachine
    let fornula = parseCTL "E (G (select -> F soda))"
    let start = State "pay" -- todo randomize
    evaluate ts fornula start `shouldBe` True
  it "always eventually soda" $ do
    ts <- vendingMachine
    let formula = parseCTL "A (F soda)"
    let start = State "pay" -- todo randomize
    evaluate ts formula start `shouldBe` False
