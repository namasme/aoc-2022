{-# LANGUAGE BinaryLiterals #-}
module Day16.FirstSpec (spec) where

import Control.Monad.Writer (runWriter)
import Data.Bits (popCount)
import qualified Data.Map as M
import Data.Map ((!))
import Day16.First
import Day16.First.Internal
import Test.Hspec

spec :: Spec
spec = do
  valves <- parseInput <$> runIO (readFile "data/Day16/testInput")

  describe "Parsing" $ do
    it "parses without errors" $ do
      length valves `shouldBe` 10
      head valves `shouldBe` Valve "AA" 0 ["DD", "II", "BB"]

  describe "Solution" $ do
    let initialPath = Path "AA" 0 30 0
    let sewer = buildSewer valves
    let getValve valveName = nameToValve sewer ! valveName

    describe "Building the sewer (context holder)" $ do
      let nonTrivialValves = discardTrivialValves valves

      it "discards the trivial valves (flow rate = 0)" $ do
        length nonTrivialValves `shouldBe` 6
        map name nonTrivialValves `shouldBe` ["BB", "CC", "DD", "EE", "HH", "JJ"]

      it "creates an encoding for a given list of valves" $ do
        let encoding = encodeValves (map name nonTrivialValves)

        length encoding `shouldBe` length nonTrivialValves
        encoding `shouldBe`
          [ ("BB", 0b000001)
          , ("CC", 0b000010)
          , ("DD", 0b000100)
          , ("EE", 0b001000)
          , ("HH", 0b010000)
          , ("JJ", 0b100000)
          ]

      it "builds the adjacency list for the given list of valves" $ do
        let result = buildAdjacencyList valves
        length result `shouldBe` sum (map (length . edges) valves)
        result `shouldStartWith` [("AA", "DD"), ("AA", "II"), ("AA", "BB")]

    it "updates the path after traveling to a neighbouring valve" $ do
      let neighbour = (1, 0b000001, getValve "BB")
      let result = updatePath neighbour initialPath

      currentValve result `shouldBe` "BB"
      currentPressure result `shouldBe` 28 * 13
      remainingTime result `shouldBe` 28
      visited result `shouldBe` 0b000001

    it "determines wheter a path has visited a given valve" $ do
      let mockPath encoded = Path undefined undefined undefined encoded

      0b000001 `unvisitedBy` mockPath 0b110110 `shouldBe` True
      0b010000 `unvisitedBy` mockPath 0b110110 `shouldBe` False

    it "finds the unvisited valves of a given path" $ do
      let path = Path "CC" undefined undefined 0b110110

      findUnvisitedValves sewer path `shouldMatchList`
        [(2, 0b001000, getValve "EE"), (1, 0b000001, getValve "BB")]

    it "explores the neighbours of a given path" $ do
      let result = stepPath sewer initialPath
      let bbPath = Path "BB" (28 * 13) 28 0b000001

      length result `shouldBe` 6
      elem bbPath result `shouldBe` True

    it "performs a full step for a list of outstanding paths" $ do
      let (pendingPaths, PathOptimizer pressures) = runWriter . step sewer . stepPath sewer $ initialPath

      length pendingPaths `shouldBe` 30
      map currentValve pendingPaths `shouldNotSatisfy` elem "AA"
      map remainingTime pendingPaths `shouldSatisfy` all (<= 30 - 2 - 2)
      map currentPressure pendingPaths `shouldSatisfy` all (>= 28 + 26)
      map (popCount . visited) pendingPaths `shouldSatisfy` all (== 2)

    it "finds the optimum pressure out of all possible paths" $ do
      findOptimumPressure sewer initialPath `shouldBe` 1651
