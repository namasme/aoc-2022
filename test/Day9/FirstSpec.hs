module Day9.FirstSpec (spec) where

import qualified Data.Set as S
import Utils.Spatial (Direction(..), Point2D(..), move)
import Day9.First
import Test.Hspec

spec :: Spec
spec = do
  motions <- parseInput <$> runIO (readFile "data/Day9/testInput")

  describe "Parsing" $ do
    it "parses without errors" $ do
      length motions `shouldBe` 8
      head motions `shouldBe` Motion R 4

  describe "Solution" $ do
    it "can break a motion into basic steps" $ do
      toSteps (Motion U 3) `shouldBe` [U, U, U]

    it "determines when the rope is too tight" $ do
      let origin = Point2D 0 0

      isRopeValid (Point2D 0 0) origin `shouldBe` True
      isRopeValid (Point2D 1 0) origin `shouldBe` True
      isRopeValid (Point2D 0 1) origin `shouldBe` True
      isRopeValid (Point2D 1 1) origin `shouldBe` True
      isRopeValid (Point2D 2 1) origin `shouldBe` False
      isRopeValid (Point2D 3 5) origin `shouldBe` False

    it "updated the rope accordingly after the head has moved" $ do
      let origin = Point2D 0 0

      updateEnd origin origin `shouldBe` origin
      updateEnd (Point2D 1 0) origin `shouldBe` origin
      updateEnd (Point2D 0 1) origin `shouldBe` origin
      updateEnd (Point2D 0 1) origin `shouldBe` origin
      updateEnd (Point2D 1 1) origin `shouldBe` origin
      updateEnd (Point2D 2 1) origin `shouldBe` Point2D 1 1

    it "can apply a single step" $ do
      let r = applyStep initialRope R
      let rr = applyStep r R
      let rru = applyStep rr U
      let rruu = applyStep rru U
      let rruul = applyStep rruu L

      r `shouldBe` Rope (Point2D 1 0) (Point2D 0 0)
      rr `shouldBe` Rope (Point2D 2 0) (Point2D 1 0)
      rru `shouldBe` Rope (Point2D 2 1) (Point2D 1 0)
      rruu `shouldBe` Rope (Point2D 2 2) (Point2D 2 1)
      rruul `shouldBe` Rope (Point2D 1 2) (Point2D 2 1)

    it "can apply all motions" $ do
      let allSteps = trackMotions motions

      length allSteps `shouldBe` 1 + sum (map steps motions)
      last allSteps `shouldBe` Rope (Point2D 2 2) (Point2D 1 2)

    it "tracks the position of the tail across motions" $ do
      let positions = findTailVisitedPositions (trackMotions motions)

      S.size positions `shouldBe` 13
