module Day1.SecondSpec (spec) where

import Day1.Second
import Test.Hspec

spec :: Spec
spec = do
  elvesCalories <- runIO (parseInput <$> readFile "data/Day1/testInput")

  describe "Solution" $ do
    it "solves the problem" $ do
      findTopThreeMostCaloriesElves elvesCalories `shouldBe` 45000
