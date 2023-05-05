module CTL.ParserSpec where

import CTL.Model
import CTL.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "state formula parser" $ do
    it "parses true" $ do
      parseCTL "true" `shouldBe` BoolLiteral True
    it "parses atomic proposition" $ do
      parseCTL "a" `shouldBe` Prop "a"
    it "parses negation" $ do
      parseCTL "!(a)" `shouldBe` Negation (Prop "a")
    it "parses conjunction" $ do
      parseCTL "(a && b)" `shouldBe` Conjunct (Prop "a") (Prop "b")
    it "parses disjunction" $ do
      parseCTL "(a || b)" `shouldBe` Negation (Conjunct (Negation (Prop "a")) (Negation (Prop "b")))
    it "parses implication" $ do
      parseCTL "(a -> b)" `shouldBe` Conjunct (Negation (Prop "a")) (Prop "b")
    it "parses equivalence" $ do
      parseCTL "(a <-> b)" `shouldBe` Conjunct (Conjunct (Negation (Prop "a")) (Prop "b")) (Conjunct (Negation (Prop "b")) (Prop "a"))
    it "parses xor" $ do
      parseCTL "(a xor b)" `shouldBe` Conjunct (Conjunct (Negation (Prop "a")) (Prop "b")) (Conjunct (Negation (Prop "b")) (Prop "a"))
    it "parses exists" $ do
      parseCTL "E (F a)" `shouldBe` Exists (Eventually (Prop "a"))
    it "parses forall" $ do
      parseCTL "A (F a)" `shouldBe` ForAll (Eventually (Prop "a"))
  describe "path formula parser" $ do
    it "parses eventually" $ do
      parseCTL "E (F a)" `shouldBe` Exists (Eventually (Prop "a"))
    it "parses always" $ do
      parseCTL "A (G a)" `shouldBe` ForAll (Always (Prop "a"))
    it "parses until" $ do
      parseCTL "E (a U b)" `shouldBe` Exists (Until (Prop "a") (Prop "b"))
