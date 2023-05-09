import qualified CTL.ParserSpec
import qualified CTL.SemanticsSpec
import qualified LTL.ParserSpec
import qualified LTL.SemanticsSpec
import qualified TS.ParserSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CTL.Parser" CTL.ParserSpec.spec
  describe "CTL.Semantics" CTL.SemanticsSpec.spec
  describe "LTL.Parser" LTL.ParserSpec.spec
  describe "LTL.Semantics" LTL.SemanticsSpec.spec
  describe "TS.Parser" TS.ParserSpec.spec
