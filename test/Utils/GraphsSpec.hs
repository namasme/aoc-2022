module Utils.GraphsSpec (spec) where

import Utils.Spatial (Point2D(..), vonNeumannNeighbours)

import Utils.Graphs
import Test.Hspec

spec :: Spec
spec = do
  describe "BFS" $ do
    it "finds the shortest path" $ do
      let isGoal = (== Point2D 3 3)
      let manifest = BFSManifest (Point2D 0 0) vonNeumannNeighbours isGoal

      runBFS manifest `shouldBe` 6
