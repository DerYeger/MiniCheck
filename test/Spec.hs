import qualified CTL.ParserSpec
import qualified TS.ParserSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CTL.Parser" CTL.ParserSpec.spec
  describe "TS.Parser" TS.ParserSpec.spec
