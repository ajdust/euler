import Prelude hiding (lookup, map)
import Test.Hspec
import Data.Map
import Data.Aeson (decode)
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as CB
import Problem01
import Problem02
import Problem03
import Problem04
import Problem05
import Problem06
import Problem07
import Problem08
import Problem09
import Problem10
import Problem11

getJSON :: IO B.ByteString
getJSON = B.readFile "../../answers.json"

getAnswers :: B.ByteString -> Map String Integer
getAnswers s =
    let decoded = decode s :: Maybe (Map String String)
    in case decoded of
        Nothing -> error "Could not decode JSON"
        Just m -> map read m :: Map String Integer

getAnswerFor :: (Map String Integer) -> String -> Integer
getAnswerFor answerMap key =
    let answer = lookup key answerMap
    in case answer of
        Nothing -> error ("Answer key not found for " ++ key)
        Just n -> n

main :: IO ()
main = do
    text <- getJSON
    let answerMap = getAnswers text
    let answerFor = getAnswerFor answerMap
    hspec $ do
        describe "Validate Problem01" $ do
            it "Problem01 solve should be correct" $ do
                Problem01.solve `shouldBe` (answerFor "1")

        describe "Validate Problem02" $ do
            it "Problem02 solve should be correct" $ do
                Problem02.solve `shouldBe` (answerFor "2")

        describe "Validate Problem03" $ do
            it "Problem03 solve should be correct" $ do
                Problem03.solve `shouldBe` (answerFor "3")

        describe "Validate Problem04" $ do
            it "Problem04 solve should be correct" $ do
                Problem04.solve `shouldBe` (answerFor "4")

        describe "Validate Problem05" $ do
            it "Problem05 solve should be correct" $ do
                Problem05.solve `shouldBe` (answerFor "5")

        describe "Validate Problem06" $ do
            it "Problem06 solve should be correct" $ do
                Problem06.solve `shouldBe` (answerFor "6")

        describe "Validate Problem07" $ do
            it "Problem07 solve should be correct" $ do
                Problem07.solve `shouldBe` (answerFor "7")

        describe "Validate Problem08" $ do
            it "Problem08 solve should be correct" $ do
                Problem08.solve `shouldBe` (answerFor "8")

        describe "Validate Problem09" $ do
            it "Problem09 solve should be correct" $ do
                Problem09.solve `shouldBe` (answerFor "9")

        describe "Validate Problem10" $ do
            it "Problem10 solve should be correct" $ do
                Problem10.solve `shouldBe` (answerFor "10")

        describe "Validate Problem11" $ do
            it "Problem11 solve should be correct" $ do
                Problem11.solve `shouldBe` (answerFor "11")

