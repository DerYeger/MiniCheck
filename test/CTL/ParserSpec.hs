module CTL.ParserSpec where

import CTL.Parser
import Test.Hspec ( describe, it, shouldBe, Spec )
import CTL.Model

spec :: Spec
spec = do
  describe "the parser" $ do
    it "parses true" $ do
      parseCTL "true" `shouldBe` (BoolLiteral True)
