module Day24.SecondSpec (spec) where

import Day24.First (parseInput)
import Day24.Second
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day24/testInput")
  let valley = parseInput testInput

  describe "Solution" $ do
    it "finds the shortest path there and back and there again" $ do
      findShortestPathThereAndBackAndThereAgain valley `shouldBe` 54
