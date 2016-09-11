import Test.Hspec
import BetaReductor

main :: IO ()
main = hspec $ do
  describe "test" $ do
    it "returns 1" $
      test 3 `shouldBe` 1
