module Day12.SecondSpec (spec) where

import Data.Map ((!))
import Utils.Graphs (BFSManifest(..), runBFS)
import Utils.Spatial (Point2D(..))
import Day12.Second
import Test.Hspec

spec :: Spec
spec = do
  (_, endPosition, grid) <- parseInput <$> runIO (readFile "data/Day12/testInput")

  describe "Solution" $ do
    it "finds the accessible neighbours of a given position in the grid" $ do
      neighbours' grid endPosition `shouldMatchList` [Point2D 4 2]
      neighbours' grid (Point2D 3 2) `shouldMatchList` [Point2D 3 1, Point2D 3 3, Point2D 4 2]

    it "finds the shortest distance to the goal" $ do
      let isGoal' grid p = grid ! p  == 0
      let manifest = BFSManifest endPosition (neighbours' grid) (isGoal' grid)

      runBFS manifest `shouldBe` 29
