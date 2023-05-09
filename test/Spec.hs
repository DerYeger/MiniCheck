import qualified CTL.ParserSpec
import qualified LTL.ParserSpec
import qualified SemanticsSpec
import qualified TS.ParserSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CTL.Parser" CTL.ParserSpec.spec
  describe "LTL.Parser" LTL.ParserSpec.spec
  describe "TS.Parser" TS.ParserSpec.spec
  describe "Semantics" SemanticsSpec.spec
