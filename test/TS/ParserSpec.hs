module TS.ParserSpec where

import Data.Set (fromList)
import TS.Model
import TS.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

vendingMachine :: TransitionSystem
vendingMachine = TS states actions transitions initialStates atomicPropositions labelingFunction
  where
    states = fromList [State "pay", State "soda", State "select", State "beer"]
    actions = fromList [Action "insert_coin", Action "τ", Action "get_soda", Action "get_beer"]
    transitions = [T (State "pay") (Action "insert_coin") (State "select"), T (State "select") (Action "τ") (State "soda"), T (State "select") (Action "τ") (State "beer"), T (State "soda") (Action "get_soda") (State "pay"), T (State "beer") (Action "get_beer") (State "pay")]
    initialStates = fromList [State "pay"]
    atomicPropositions = fromList [AtomicProposition "soda", AtomicProposition "beer", AtomicProposition "pay", AtomicProposition "select"]
    labelingFunction _ = []

spec :: Spec
spec = describe "the parser" $ do
  it "parses the example" $ do
    file <- readFile "examples/vending-machine.txt"
    let ts = parseTS file
    ts `shouldBe` vendingMachine
