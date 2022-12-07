module Day4.SecondSpec (spec) where

import Day4.Second
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO $ readFile "data/Day4/testInput"
  let intervalPairs = parseInput testInput

  describe "Solution" $ do
    it "determines if two intervals overlap" $ do
      overlap (Interval 0 5) (Interval 1 6) `shouldBe` True
      overlap (Interval 0 5) (Interval 6 7) `shouldBe` False
      overlap (Interval 0 5) (Interval (-3) (-1)) `shouldBe` False

    it "finds all overlapping interval pairs" $ do
      length (findAllOverlappingIntervalPairs intervalPairs) `shouldBe` 4
