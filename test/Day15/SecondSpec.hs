module Day15.SecondSpec (spec) where

import Utils.Spatial (Point2D(..))

import Day15.Second
import Test.Hspec

spec :: Spec
spec = do
  (sensors, beacons) <- parseInput <$> runIO (readFile "data/Day15/testInput")
  let scanArea = ScanArea 0 20 0 20

  describe "Solution" $ do
    it "computes the sections for several rows" $ do
      let scanArea' = ScanArea 0 25 9 11

      computeSections scanArea' sensors `shouldBe`
        [[(-1, 23)], [(-2, 24)], [(-3, 13), (15, 25)]]

    it "intersects intervals" $ do
      intersectInterval (0, 5) (2, 6) `shouldBe` Just (2, 5)
      intersectInterval (2, 6) (0, 5) `shouldBe` Just (2, 5)
      intersectInterval (0, 2) (5, 6) `shouldBe` Nothing
      intersectInterval (2, 5) (0, 6) `shouldBe` Just (2, 5)

    it "finds the (only) missing point in an intersected section" $ do
      let intervals = [(-3, 13), (15, 25)]
      let row = 11

      findMissingPoint scanArea (intervals, row) `shouldBe` Point2D 14 11

    it "determines whether the scan area is contained in a section" $ do
      [(-1, 23)] `shouldSatisfy` containsScanArea scanArea
      [(1, 23)] `shouldNotSatisfy` containsScanArea scanArea
      [(-1, 18)] `shouldNotSatisfy` containsScanArea scanArea
      [(-20, -6), (-1, 23), (25, 40)] `shouldSatisfy` containsScanArea scanArea

    it "finds the (only) incomplete row" $ do
      let sections = computeSections scanArea sensors

      findIncompleteRow scanArea sections `shouldBe` ([(0, 13), (15, 20)], 11)

    it "finds the distress beacon" $ do
      let sections = computeSections scanArea sensors

      findDistressBeacon scanArea sections `shouldBe` Point2D 14 11

    it "calculates the tuning frequency" $ do
      tuningFrequency (Point2D 14 11) `shouldBe` 56000011
