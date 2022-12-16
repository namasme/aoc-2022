module Day12.FirstSpec (spec) where

import Data.Map ((!))
import qualified Data.Map as M
import Utils.Graphs (BFSManifest(..), runBFS)
import Utils.Spatial (Point2D(..))

import Day12.First
import Test.Hspec

spec :: Spec
spec = do
  (startPosition, endPosition, grid) <- parseInput <$> runIO (readFile "data/Day12/testInput")

  describe "Parsing" $ do
    it "maps an index to coordinates" $ do
      toCoordinates 5 0 `shouldBe` Point2D 0 0
      toCoordinates 5 3 `shouldBe` Point2D 3 0
      toCoordinates 5 12 `shouldBe` Point2D 2 2

    it "parses without errors" $ do
      M.size grid `shouldBe` 40
      startPosition `shouldBe` Point2D 0 0
      endPosition `shouldBe` Point2D 5 2

      grid ! startPosition `shouldBe` 0
      grid ! endPosition `shouldBe` 25

  describe "Solution" $ do
    it "finds the accessible neighbours of a given position in the grid" $ do
      neighbours' grid startPosition `shouldMatchList` [Point2D 1 0, Point2D 0 1]
      neighbours' grid (Point2D 4 3) `shouldMatchList` [Point2D 3 3, Point2D 5 3, Point2D 4 4]

    it "finds the shortest distance to the goal" $ do
      let isGoal' = (== endPosition)
      let manifest = BFSManifest startPosition (neighbours' grid) isGoal'

      runBFS manifest `shouldBe` 31
