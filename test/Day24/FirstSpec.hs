module Day24.FirstSpec (spec) where

import qualified Data.Map as M
import Data.Map ((!))
import Utils.Spatial (Point2D(..))
import Day24.First
import Test.Hspec
import Utils.Graphs (AStarResult(..))

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day24/testInput")
  let valley = parseInput testInput

  describe "Parsing" $ do
    it "parses blizzards" $ do
      parseBlizzard 1 2 '>' `shouldBe` Right (1, Blizzard 2 Forwards)
      parseBlizzard 1 2 '^' `shouldBe` Left (2, Blizzard 1 Backwards)

    it "identifies the dimensions of the valley" $ do
      width valley `shouldBe` 6
      height valley `shouldBe` 4

    it "detects all the blizzards" $ do
      let totalElems = sum . map length . M.elems
      let totalHorizontal = totalElems (horizontalBlizzards valley)
      let totalVertical = totalElems (verticalBlizzards valley)

      totalHorizontal `shouldBe` 13
      totalVertical `shouldBe` 6

      horizontalBlizzards valley ! 4 `shouldMatchList`
        map (uncurry Blizzard) [(1, Backwards), (6, Forwards)]
      verticalBlizzards valley ! 2 `shouldMatchList`
        map (uncurry Blizzard) [(3, Forwards), (4, Backwards)]

  describe "Solution" $ do
    it "predicts the position of a blizzard at a given time" $ do
      let _width = 6
      let blizzard = Blizzard 2 Forwards

      blizzardPositionAt _width 0 blizzard `shouldBe` initialPosition blizzard
      blizzardPositionAt _width 1 blizzard `shouldBe` 3
      blizzardPositionAt _width 4 blizzard `shouldBe` 6
      blizzardPositionAt _width 5 blizzard `shouldBe` 1

    it "identifies key locations" $ do
      valleyEntrance valley `shouldBe` Point2D 0 1
      valleyExit valley `shouldBe` Point2D 5 6

    it "determines whether a given position is within the valley" $ do
      valleyEntrance valley `shouldSatisfy` isWithinBounds valley
      valleyExit valley `shouldSatisfy` isWithinBounds valley
      Point2D 3 4 `shouldSatisfy` isWithinBounds valley
      Point2D 0 4 `shouldNotSatisfy` isWithinBounds valley
      Point2D 5 0 `shouldNotSatisfy` isWithinBounds valley

    it "determines whether a position is free (of blizzards) at a given time" $ do
      isFree valley (Point2D 1 3) 0 `shouldBe` True
      isFree valley (Point2D 1 1) 0 `shouldBe` False
      isFree valley (Point2D 2 1) 18 `shouldBe` True
      isFree valley (Point2D 2 4) 18 `shouldBe` False

    it "finds the shortest path to the exit" $ do
      findShortestPath valley `shouldBe` 18
