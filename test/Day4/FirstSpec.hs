module Day4.FirstSpec (spec) where

import Day4.First
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO $ readFile "data/Day4/testInput"
  let intervalPairs = parseInput testInput

  describe "Parsing" $ do
    it "parses without errors" $ do
      head intervalPairs `shouldBe` (Interval 2 4, Interval 6 8)

  describe "Solution" $ do
    it "determines if one interval fully contains another" $ do
      contains (Interval 0 5) (Interval 1 3) `shouldBe` True
      contains (Interval 0 5) (Interval 1 6) `shouldBe` False
      contains (Interval 1 5) (Interval 0 3) `shouldBe` False

    it "determines if either interval fully contains the other" $ do
      eitherContains (Interval 0 5) (Interval 1 3) `shouldBe` True
      eitherContains (Interval 1 3) (Interval 0 5) `shouldBe` True
      eitherContains (Interval 0 5) (Interval 1 6) `shouldBe` False

    it "finds all interval pairs where one fully contains the other" $ do
      length (findAllContainingIntervalPairs intervalPairs) `shouldBe` 2
