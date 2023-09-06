module Day22.FirstSpec (spec) where

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import Utils.Spatial (Direction(..), Point2D(..), RotationalDirection(..))
import Day22.First
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day22/testInput")
  let (startingPosition, chart, path) = parseInput testInput
  let initialPlayer = buildInitialPlayer startingPosition
  let buildPlayer (i, j, _facing) = Player (Point2D i j) _facing

  describe "Parsing" $ do
    it "identifies starting position" $ do
      startingPosition `shouldBe` Point2D 0 8

    describe "Chart parsing" $ do
      it "identifies the boundaries" $ do
        M.size (rowBoundaries chart) `shouldBe` 12
        M.size (columnBoundaries chart) `shouldBe` 16

        rowBoundaries chart ! 9 `shouldBe` (8, 15)
        columnBoundaries chart ! 3 `shouldBe` (4, 7)

      it "finds the walls" $ do
        let actualWalls = walls chart
        S.size actualWalls `shouldBe` 13
        S.member (Point2D 0 11) actualWalls `shouldBe` True
        S.member (Point2D 11 14) actualWalls `shouldBe` True
        S.member (Point2D 4 0) actualWalls `shouldBe` False

    it "parses the path" $ do
      length path `shouldBe` 13
      path `shouldStartWith` [Move 10, Turn CW, Move 5, Turn CCW]

  describe "Solution" $ do
    it "loops from a given number wrapping around the boundaries" $ do
      loop 3 (1, 6) `shouldStartWith` concat [[3 .. 6], [1 .. 6], [1 .. 6]]
      loop 2 (4, 0) `shouldStartWith` [2, 1, 0, 4, 3, 2, 1, 0, 4, 3, 2]

    it "identifies the relevant boundaries for the current player" $ do
      getBoundaries chart (buildPlayer (9, 9, R)) `shouldBe` (8, 15)
      getBoundaries chart (buildPlayer (9, 9, L)) `shouldBe` (15, 8)
      getBoundaries chart (buildPlayer (9, 9, U)) `shouldBe` (11, 0)
      getBoundaries chart (buildPlayer (9, 9, D)) `shouldBe` (0, 11)

    it "generates a path segment" $ do
      pathSegment chart (buildPlayer (9, 9, R)) 10 `shouldBe`
        [Point2D 9 column | column <- [9 .. 15] ++ [8 .. 11]]

    it "applies a step" $ do
      let expectedSteps = initialPlayer : map buildPlayer
            [ (0, 10, R)
            , (0, 10, D)
            , (5, 10, D)
            , (5, 10, R)
            , (5, 3, R)
            , (5, 3, D)
            , (7, 3, D)
            , (7, 3, R)
            , (7, 7, R)
            , (7, 7, D)
            , (5, 7, D)
            , (5, 7, R)
            , (5, 7, R)
            ]

      scanl (applyStep chart) initialPlayer path `shouldBe` expectedSteps

    it "applies a path" $ do
      applyPath chart initialPlayer path `shouldBe` buildPlayer (5, 7, R)

    it "calculates the final password" $ do
      calculatePassword (buildPlayer (5, 7, R)) `shouldBe` 6032
