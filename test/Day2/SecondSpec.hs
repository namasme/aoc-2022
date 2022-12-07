module Day2.SecondSpec (spec) where

import Day2.Second
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO $ readFile "data/Day2/testInput"
  let rawMatches = parseInput testInput

  describe "Solution" $ do
    it "calculates the score you should get given the opponent's shape and the expected result" $ do
      matchScore Rock Draw `shouldBe` shapeScore Rock + resultScore Draw
      matchScore Paper Loss `shouldBe` shapeScore Rock + resultScore Loss
      matchScore Scissors Win `shouldBe` shapeScore Rock + resultScore Win

    it "simulates all matches" $ do
      playMatches rawMatches `shouldBe` [4, 1, 7]
