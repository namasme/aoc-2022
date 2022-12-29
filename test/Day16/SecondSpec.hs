{-# LANGUAGE BinaryLiterals #-}
module Day16.SecondSpec (spec) where

import Day16.Second
import Test.Hspec

spec :: Spec
spec = do
  valves <- parseInput <$> runIO (readFile "data/Day16/testInput")

  describe "Solution" $ do
    it "determines whether two paths (or visited valves thereof) are disjoint" $ do
      areDisjoint 0b101010 0b010101 `shouldBe` True
      areDisjoint 0b000000 0b111111 `shouldBe` True
      areDisjoint 0b111000 0b001111 `shouldBe` False

    it "finds the optimum pressure out of all possible paths for both me and the elephant" $ do
      let sewer = buildSewer valves
      let initialPath = Path "AA" 0 26 0

      findOptimumPressure sewer initialPath `shouldBe` 1707
