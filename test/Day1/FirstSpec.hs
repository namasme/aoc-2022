module Day1.FirstSpec (spec) where

import Day1.First
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day1/testInput")
  let elvesCalories = parseInput testInput

  describe "Parsing" $ do
    it "parses without errors" $ do
      elvesCalories `shouldBe`
        [[1000, 2000, 3000], [4000], [5000, 6000], [7000, 8000, 9000], [10000]]

  describe "Solution" $ do
    it "solves the problem" $ do
      findMostCaloriesElf elvesCalories `shouldBe` 24000
