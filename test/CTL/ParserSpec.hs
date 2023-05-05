module CTL.ParserSpec where

import CTL.Model
import CTL.Parser
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "the parser" $ do
    it "parses true" $ do
      parseCTL "true" `shouldBe` BoolLiteral True
