module Utils.GraphsSpec (spec) where

import qualified Data.Map as M
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

  describe "Distance matrix" $ do
    it "collects all nodes in a graph from an adjacency list" $ do
      let nodes = collectNodes [(10, 20), (20, 30), (20, 40), (10, 30), (50, 60)]
      nodes `shouldMatchList` [10, 20, 30, 40, 50, 60]

    it "uses an intermediate node to join known paths" $ do
      let knownPaths =
            [ (buildKey 20 10, 1)
            , (buildKey 30 20, 2)
            , (buildKey 20 40, 3)
            , (buildKey 10 40, 1)
            , (buildKey 10 50, 10)
            , (buildKey 20 50, 1)
            , (buildKey 70 60, 100)
            ]
      let nodes = collectNodes (map fst knownPaths)
      let result = bridgeBy nodes (M.fromList knownPaths) 20
      let newDistance = getDistance' result

      newDistance 10 30 `shouldBe` Just 3 -- previously unconnected
      newDistance 10 40 `shouldBe` Just 1 -- previously connected through a better path
      newDistance 10 50 `shouldBe` Just 2 -- previously connected through a worse path
      newDistance 10 70 `shouldBe` Nothing -- still disconnected

    it "builds a distance matrix from an adjacency list" $ do
      let adjacencyList = [(10, 20), (20, 30), (30, 40), (20, 50), (60, 70)]
      let distanceMatrix = fromAdjacencyList adjacencyList
      let distance = getDistance distanceMatrix

      M.size (distances distanceMatrix) `shouldBe` (5 * 4 `div` 2) + (2 * 1 `div` 2)
      [distance u v | (u, v) <- adjacencyList] `shouldSatisfy` all (== Just 1)
      distance 10 50 `shouldBe` Just 2
      distance 10 20 `shouldBe` Just 1
      distance 10 40 `shouldBe` Just 3
      distance 10 60 `shouldBe` Nothing
