import Test.Hspec
import qualified CTL.ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CTL.Parser" CTL.ParserSpec.spec
