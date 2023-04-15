module Day20.SecondSpec (spec) where

import Day20.Second
import Test.Hspec

spec :: Spec
spec = do
  values <- parseInput <$> runIO (readFile "data/Day20/testInput")

  describe "Solution" $ do
    it "calculates the grove coordinates" $ do
      findGroveCoordinates values `shouldBe` 1623178306
