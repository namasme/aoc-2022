module Day14.FirstSpec (spec) where

import Data.Maybe (isJust, isNothing)
import qualified Data.Set as S
import Utils.Spatial (Point2D(..))

import Day14.First
import Test.Hspec

spec :: Spec
spec = do
  endss <- parseInput <$> runIO (readFile "data/Day14/testInput")

  describe "Parsing" $ do
    it "can parse a point" $ do
      parsePoint "23,45" `shouldBe` Point2D 23 45

    it "can parse a list of ends" $ do
      parseLine "498,4 -> 498,6 -> 496,6" `shouldBe`
        [Point2D 498 4, Point2D 498 6, Point2D 496 6]

    it "parses without errors" $ do
      length endss `shouldBe` 2
      map length endss `shouldBe` [3, 4]
      head (head endss) `shouldBe` Point2D 498 4

  describe "Solution" $ do
    let _source = Point2D 500 0
    let cave = buildCave _source endss

    it "builds a cave" $ do
      abyssDepth cave `shouldBe` 9

    it "fills a segment" $ do
      fillSegment (Point2D 0 0) (Point2D 3 0) `shouldBe`
        [Point2D column 0 | column <- [0 .. 3]]

    it "fills a path" $ do
      (length . fillPath . head) endss `shouldBe` 5
      (length . fillPath . last) endss `shouldBe` 15

    it "determines whether a unit of sand will come to rest" $ do
      evolveSand cave _source `shouldSatisfy` isJust
      evolveSand cave (Point2D 505 0) `shouldSatisfy` isNothing

    it "determines where a unit of sand will come to rest" $ do
      evolveSand cave _source `shouldBe` Just (Point2D 500 8)

    it "can add a unit of sand to the cave" $ do
      let result = spawnSand cave
      S.member (Point2D 500 8) . blocked <$> result `shouldBe` Just True

    it "finds how many sand units till overflow" $ do
      tillOverflow cave `shouldBe` 24
