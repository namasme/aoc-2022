module Day18.SecondSpec (spec) where

import Day18.Second
import Utils.Spatial (Point3D(..))
import Test.Hspec

spec :: Spec
spec = do
  cubes <- parseInput <$> runIO (readFile "data/Day18/testInput")
  describe "Solution" $ do
    it "determines whether a value is within bounds" $ do
      2 `shouldSatisfy` isValidDimensionValue (2, 7)
      1 `shouldNotSatisfy` isValidDimensionValue (2, 7)
      9 `shouldNotSatisfy` isValidDimensionValue (2, 7)

    it "determines whether a point is within bounds" $ do
      let bounds = Bounds (2, 4) (3, 7) (1, 5)

      Point3D 2 3 1 `shouldSatisfy` isValid bounds
      Point3D 2 0 1 `shouldNotSatisfy` isValid bounds
      Point3D 6 3 1 `shouldNotSatisfy` isValid bounds
      Point3D 2 3 7 `shouldNotSatisfy` isValid bounds

    it "calculates the bounds for a given list of cubes" $ do
      let expectedBounds =
            Bounds {xBounds = (0, 4), yBounds = (0, 4), zBounds = (0, 7)}
      let actualBounds = calculateBounds cubes

      actualBounds `shouldBe` expectedBounds

    it "generates the neighbours of a given point within the bounds" $ do
      let bounds = calculateBounds cubes

      neighbours bounds (Point3D 0 0 0) `shouldBe` [Point3D 1 0 0, Point3D 0 1 0, Point3D 0 0 1]

    it "calculates the external surface of the droplet" $ do
      calculateExternalSurface cubes `shouldBe` 58
