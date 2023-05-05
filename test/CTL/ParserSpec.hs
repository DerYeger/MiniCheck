module CTL.ParserSpec (spec) where

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
      parseCTL "(a || b)" `shouldBe` transformDisjunction (Prop "a") (Prop "b")
    it "parses implication" $ do
      parseCTL "(a -> b)" `shouldBe` transformImplication (Prop "a") (Prop "b")
    it "parses equivalence" $ do
      parseCTL "(a <-> b)" `shouldBe` transformEquivalence (Prop "a") (Prop "b")
    it "parses xor" $ do
      parseCTL "(a xor b)" `shouldBe` transformXor (Prop "a") (Prop "b")
    it "parses exists" $ do
      parseCTL "E (F a)" `shouldBe` Exists (transformEventually (Prop "a"))
    it "parses forall" $ do
      parseCTL "A (F a)" `shouldBe` ForAll (transformEventually (Prop "a"))
    it "derives disjunction" $ do
      parseCTL "(a || b)" `shouldBe` Negation (Conjunct (Negation (Prop "a")) (Negation (Prop "b")))
    it "derives implication" $ do
      parseCTL "(a -> b)" `shouldBe` Conjunct (Negation (Prop "a")) (Prop "b")
    it "derives equivalence" $ do
      parseCTL "(a <-> b)" `shouldBe` Conjunct (Conjunct (Negation (Prop "a")) (Prop "b")) (Conjunct (Negation (Prop "b")) (Prop "a"))
    it "derives xor" $ do
      parseCTL "(a xor b)" `shouldBe` Negation (Conjunct (Negation (Conjunct (Prop "a") (Negation (Prop "b")))) (Negation (Conjunct (Prop "b") (Negation (Prop "a")))))
  describe "path formula parser" $ do
    it "parses eventually" $ do
      parseCTL "E (F a)" `shouldBe` Exists (transformEventually (Prop "a"))
    it "parses always" $ do
      parseCTL "A (G a)" `shouldBe` Negation (ForAll (transformAlways (Prop "a")))
    it "parses until" $ do
      parseCTL "E (a U b)" `shouldBe` Exists (Until (Prop "a") (Prop "b"))
    it "derives exists eventually" $ do
      parseCTL "E (F a)" `shouldBe` Exists (Until (BoolLiteral True) (Prop "a"))
    it "derives forall eventually" $ do
      parseCTL "A (F a)" `shouldBe` ForAll (Until (BoolLiteral True) (Prop "a"))
    it "derives exists always" $ do
      parseCTL "E (G a)" `shouldBe` Negation (Exists (Until (BoolLiteral True) (Negation (Prop "a"))))
    it "derives forall always" $ do
      parseCTL "A (G a)" `shouldBe` Negation (ForAll (Until (BoolLiteral True) (Negation (Prop "a"))))
