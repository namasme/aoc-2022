module Day18.FirstSpec (spec) where

import Day18.First
import Utils.Spatial (Point3D(..))
import Test.Hspec

spec :: Spec
spec = do
  cubes <- parseInput <$> runIO (readFile "data/Day18/testInput")

  describe "Parsing" $ do
    it "parses without errors" $ do
      length cubes `shouldBe` 13
      head cubes `shouldBe` Point3D 2 2 2
      last cubes `shouldBe` Point3D 2 3 5

  describe "Solution" $ do
    it "calculates L1 norm" $ do
      l1 (Point3D 3 4 7) `shouldBe` 14

    it "counts how many faces two cubes share" $ do
      sharedFaces (Point3D 1 1 1) (Point3D 2 1 1) `shouldBe` 1
      sharedFaces (Point3D 1 1 1) (Point3D 0 0 0) `shouldBe` 0

    it "counts how many faces a list of cubes share" $ do
      countSharedFaces cubes `shouldBe` 7

    it "calculates total surface" $ do
      countVisibleFaces cubes `shouldBe` 64
