import Test.Hspec
import Problem01
import Problem02

main :: IO ()
main = hspec $ do

  describe "Validate Problem01" $ do
    it "Problem01 solve should result in 233168" $ do
      Problem01.solve `shouldBe` 233168

  describe "Validate Problem02" $ do
  	it "Problem02 solve should result in 4613732" $ do
  	  Problem02.solve `shouldBe` 4613732