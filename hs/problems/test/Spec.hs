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
            it "Probelem06 solve should be correct" $ do
                Problem06.solve `shouldBe` (answerFor "6")