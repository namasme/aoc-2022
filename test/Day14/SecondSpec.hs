module Day14.SecondSpec (spec) where


import qualified Data.Set as S
import Utils.Spatial (Point2D(..))

import Day14.Second
import Test.Hspec

spec :: Spec
spec = do
  endss <- parseInput <$> runIO (readFile "data/Day14/testInput")
  let _source = Point2D 500 0
  let cave = buildCave' _source endss

  describe "Solution" $ do
    it "builds a cave aware of the bottom layer of rock" $ do
      abyssDepth cave `shouldBe` 11

    it "determines where a unit of sand will come to rest" $ do
      evolveSand cave _source `shouldBe` Point2D 500 8

    it "can add a unit of sand to the cave" $ do
      let result = spawnSand cave
      (S.member (Point2D 500 8) . blocked) result `shouldBe` True

    it "finds how many sand units till the source is blocked" $ do
      tillSource cave `shouldBe` 93
