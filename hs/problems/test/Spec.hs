import Test.Hspec
import Problem01

main :: IO ()
main = hspec $ do

  describe "Validate Problem01" $ do
    it "Problem01 solve should result in 233168" $ do
      solve `shouldBe` 233168