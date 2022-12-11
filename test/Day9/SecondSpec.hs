module Day9.SecondSpec (spec) where

import qualified Data.Set as S
import Utils.Spatial (Direction(..), Point2D(..))
import Day9.First (steps)

import Day9.Second
import Test.Hspec

spec :: Spec
spec = do
  motions <- parseInput <$> runIO (readFile "data/Day9/testInput")
  motions2 <- parseInput <$> runIO (readFile "data/Day9/testInput2")

  describe "Solution" $ do
    it "can apply a single step for a knotted rope" $ do
      let origin = Point2D 0 0
      let tenseRope = [Point2D x 0 | x <- [0 .. 9]]

      applyStep initialRope R `shouldBe` Point2D 1 0 : replicate 9 origin
      applyStep tenseRope L `shouldBe` [Point2D x 0 | x <- [-1 .. 8]]

    it "can apply a single step for a knotted rope" $ do
      let origin = Point2D 0 0
      let tenseRope = [Point2D x 0 | x <- [0 .. 9]]

      applyStep initialRope R `shouldBe` Point2D 1 0 : replicate 9 origin
      applyStep tenseRope L `shouldBe` [Point2D x 0 | x <- [-1 .. 8]]

    it "can apply all motions" $ do
      let allSteps = trackMotions motions
      let finalState = last allSteps

      length allSteps `shouldBe` 1 + sum (map steps motions)
      head finalState `shouldBe` Point2D 2 2
      last finalState `shouldBe` Point2D 0 0

    it "can apply all motions (harder case)" $ do
      let allSteps = trackMotions motions2
      let finalState = last allSteps

      finalState `shouldBe` [Point2D (-11) y | y <- [15, 14 .. 6]]

    it "tracks the positions of the tail across motions" $ do
      let positions = findTailVisitedPositions (trackMotions motions2)

      S.size positions `shouldBe` 36
