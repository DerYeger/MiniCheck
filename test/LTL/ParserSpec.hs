module LTL.ParserSpec (spec) where

import LTL.Model
import LTL.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "state formula parser" $ do
    it "parses true" $ do
      parseLTL "true" `shouldBe` Right (BoolLiteral True)
    it "parses atomic proposition" $ do
      parseLTL "a" `shouldBe` Right (Prop "a")
    it "parses negation" $ do
      parseLTL "!(a)" `shouldBe` Right (Negation (Prop "a"))
    it "parses conjunction" $ do
      parseLTL "(a && b)" `shouldBe` Right (Conjunct (Prop "a") (Prop "b"))
    it "parses disjunction" $ do
      parseLTL "(a || b)" `shouldBe` Right (transformDisjunction (Prop "a") (Prop "b"))
    it "parses implication" $ do
      parseLTL "(a -> b)" `shouldBe` Right (transformImplication (Prop "a") (Prop "b"))
    it "parses equivalence" $ do
      parseLTL "(a <-> b)" `shouldBe` Right (transformEquivalence (Prop "a") (Prop "b"))
    it "parses xor" $ do
      parseLTL "(a xor b)" `shouldBe` Right (transformXor (Prop "a") (Prop "b"))
    it "derives disjunction" $ do
      parseLTL "(a || b)" `shouldBe` Right (Negation (Conjunct (Negation (Prop "a")) (Negation (Prop "b"))))
    it "derives implication" $ do
      parseLTL "(a -> b)" `shouldBe` Right (Negation (Conjunct (Negation (Negation (Prop "a"))) (Negation (Prop "b"))))
    it "derives equivalence" $ do
      parseLTL "(a <-> b)" `shouldBe` Right (Conjunct (Negation (Conjunct (Negation (Negation (Prop "a"))) (Negation (Prop "b")))) (Negation (Conjunct (Negation (Negation (Prop "b"))) (Negation (Prop "a")))))
    it "derives xor" $ do
      parseLTL "(a xor b)" `shouldBe` Right (Negation (Conjunct (Negation (Conjunct (Prop "a") (Negation (Prop "b")))) (Negation (Conjunct (Prop "b") (Negation (Prop "a"))))))
  describe "path formula parser" $ do
    it "parses next" $ do
      parseLTL "(X a)" `shouldBe` Right (Next (Prop "a"))
    it "parses eventually" $ do
      parseLTL "(F a)" `shouldBe` Right (transformEventually (Prop "a"))
    it "parses until" $ do
      parseLTL "(a U b)" `shouldBe` Right (Until (Prop "a") (Prop "b"))
