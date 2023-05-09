module CTL.ParserSpec (spec) where

import CTL.Model
import CTL.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "state formula parser" $ do
    it "parses true" $ do
      parseCTL "true" `shouldBe` Right (BoolLiteral True)
    it "parses atomic proposition" $ do
      parseCTL "a" `shouldBe` Right (Prop "a")
    it "parses negation" $ do
      parseCTL "!(a)" `shouldBe` Right (Negation (Prop "a"))
    it "parses conjunction" $ do
      parseCTL "(a && b)" `shouldBe` Right (Conjunct (Prop "a") (Prop "b"))
    it "parses disjunction" $ do
      parseCTL "(a || b)" `shouldBe` Right (transformDisjunction (Prop "a") (Prop "b"))
    it "parses implication" $ do
      parseCTL "(a -> b)" `shouldBe` Right (transformImplication (Prop "a") (Prop "b"))
    it "parses equivalence" $ do
      parseCTL "(a <-> b)" `shouldBe` Right (transformEquivalence (Prop "a") (Prop "b"))
    it "parses xor" $ do
      parseCTL "(a xor b)" `shouldBe` Right (transformXor (Prop "a") (Prop "b"))
    it "parses exists" $ do
      parseCTL "E (F a)" `shouldBe` Right (Exists (transformEventually (Prop "a")))
    it "parses forall" $ do
      parseCTL "A (F a)" `shouldBe` Right (ForAll (transformEventually (Prop "a")))
    it "derives disjunction" $ do
      parseCTL "(a || b)" `shouldBe` Right (Negation (Conjunct (Negation (Prop "a")) (Negation (Prop "b"))))
    it "derives implication" $ do
      parseCTL "(a -> b)" `shouldBe` Right (Negation (Conjunct (Negation (Negation (Prop "a"))) (Negation (Prop "b"))))
    it "derives equivalence" $ do
      parseCTL "(a <-> b)" `shouldBe` Right (Conjunct (Negation (Conjunct (Negation (Negation (Prop "a"))) (Negation (Prop "b")))) (Negation (Conjunct (Negation (Negation (Prop "b"))) (Negation (Prop "a")))))
    it "derives xor" $ do
      parseCTL "(a xor b)" `shouldBe` Right (Negation (Conjunct (Negation (Conjunct (Prop "a") (Negation (Prop "b")))) (Negation (Conjunct (Prop "b") (Negation (Prop "a"))))))
  describe "path formula parser" $ do
    it "parses next" $ do
      parseCTL "E (X a)" `shouldBe` Right (Exists (Next (Prop "a")))
    it "parses eventually" $ do
      parseCTL "E (F a)" `shouldBe` Right (Exists (transformEventually (Prop "a")))
    it "parses always" $ do
      parseCTL "A (F a)" `shouldBe` Right (ForAll (Until (BoolLiteral True) (Prop "a")))
    it "parses until" $ do
      parseCTL "E (a U b)" `shouldBe` Right (Exists (Until (Prop "a") (Prop "b")))
    it "derives exists eventually" $ do
      parseCTL "E (F a)" `shouldBe` Right (Exists (Until (BoolLiteral True) (Prop "a")))
    it "derives forall eventually" $ do
      parseCTL "A (F a)" `shouldBe` Right (ForAll (Until (BoolLiteral True) (Prop "a")))
    it "derives exists always" $ do
      parseCTL "E (G a)" `shouldBe` Right (Negation (ForAll (Until (BoolLiteral True) (Negation (Prop "a")))))
    it "derives forall always" $ do
      parseCTL "A (G a)" `shouldBe` Right (Negation (Exists (Until (BoolLiteral True) (Negation (Prop "a")))))
