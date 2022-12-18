module Day15.FirstSpec (spec) where

import Utils.Spatial (Point2D(..))
import Day15.First
import Test.Hspec

spec :: Spec
spec = do
  (sensors, beacons) <- parseInput <$> runIO (readFile "data/Day15/testInput")

  describe "Parsing" $ do
    it "parses without errors" $ do
      length sensors `shouldBe` 14
      length beacons `shouldBe` 14
      head sensors `shouldBe` Sensor (Point2D 2 18) 7
      head beacons `shouldBe` Point2D (-2) 15

  describe "Solution" $ do
    it "takes a row slice off a sensor's coverage" $ do
      let sensor = sensors !! 6

      rowSlice 10 sensor `shouldBe` Just (2, 14)
      rowSlice 20 sensor `shouldBe` Nothing

    it "determines if a value lies in a given interval" $ do
      liesIn 2 (1, 3) `shouldBe` True
      liesIn 4 (1, 3) `shouldBe` False

    it "merges intervals" $ do
      intervalUnion [(0, 3), (7, 9), (2, 5)] `shouldBe` [(0, 5), (7, 9)]

    it "finds the combined size of a collection of disjoint intervals" $ do
      combinedSize [(1, 3), (5, 7), (9, 9)] `shouldBe` 7

    it "computes a section" $ do
      computeSection 9 sensors `shouldBe` [(-1, 23)]
      computeSection 10 sensors `shouldBe` [(-2, 24)]
      computeSection 11 sensors `shouldBe` [(-3, 13), (15, 25)]

    it "finds how many beacons in a collection of slices" $ do
      let beaconsInRow row = beaconsInSectionCount row beacons (computeSection row sensors)

      beaconsInRow 9 `shouldBe` 0
      beaconsInRow 10 `shouldBe` 1
      beaconsInRow 11 `shouldBe` 0

    it "counts how many positions in a row cannot contain a beacon" $ do
      impossibleBeaconsCount 9 sensors beacons `shouldBe` 25
      impossibleBeaconsCount 10 sensors beacons `shouldBe` 26
      impossibleBeaconsCount 11 sensors beacons `shouldBe` 28
