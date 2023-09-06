module Day22.SecondSpec (spec) where

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import Utils.Spatial (Direction(..), Point2D(..))
import Day22.First (Player(..), buildInitialPlayer)
import Day22.Second
import Test.Hspec
import Day22.CubeParser (Atlas(..))

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day22/testInput")
  let (startingPosition, atlas, path) = parseInput testInput

  describe "Parsing" $ do
    it "identifies starting position" $ do
      startingPosition `shouldBe` Point2D 0 8

    describe "Atlas parsing" $ do
      it "identifies the cube face size" $ do
        cubeFaceSize atlas `shouldBe` 4

      it "calculates the boundaries" $ do
        let actualBoundaries = boundaries atlas
        M.size actualBoundaries `shouldBe` 6
        actualBoundaries ! 3 `shouldBe` (Point2D 0 8, Point2D 3 11)
        actualBoundaries ! 10 `shouldBe` (Point2D 8 12, Point2D 11 15)

      it "connects the faces" $ do
        let actualBorders = borders atlas
        M.size actualBorders `shouldBe` 24
        actualBorders ! (3, L) `shouldBe` (5, U)
        actualBorders ! (5, U) `shouldBe` (3, L)
        actualBorders ! (4, L) `shouldBe` (10, D)
        actualBorders ! (10, R) `shouldBe` (3, R)
        actualBorders ! (6, R) `shouldBe` (10, U)

      it "finds the walls" $ do
        let actualWalls = walls atlas
        S.size actualWalls `shouldBe` 13
        S.member (Point2D 0 11) actualWalls `shouldBe` True
        S.member (Point2D 11 14) actualWalls `shouldBe` True
        S.member (Point2D 4 0) actualWalls `shouldBe` False

  describe "Solution" $ do
    it "finds the face of a given position" $ do
      findFace atlas (Point2D 0 10) `shouldBe` 3
      findFace atlas (Point2D 5 2) `shouldBe` 4
      findFace atlas (Point2D 10 13) `shouldBe` 10

    it "detects whether the current position lies in the border of its face" $ do
      let predicate position direction = isFaceBorder atlas (Player position direction)
      let bordersFor position = filter (predicate position) [L, U, R, D]

      bordersFor (Point2D 0 8) `shouldMatchList` [L, U]
      bordersFor (Point2D 7 7) `shouldMatchList` [R, D]
      bordersFor (Point2D 10 13) `shouldMatchList` []

    it "completes a segment until the border of the current face" $ do
      let assertCompleteFace player expectedPositions =
            completeFace atlas player `shouldBe`
            [Player position (facing player) | position <- expectedPositions]

      assertCompleteFace
        (Player (Point2D 1 9) R)
        [Point2D 1 9, Point2D 1 10, Point2D 1 11]
      assertCompleteFace (Player (Point2D 1 9) L) [Point2D 1 9, Point2D 1 8]

    it
      "translates a (local) position after applying the required transformation" $ do
      let cubeFaceSize = 10
      --   0123456789
      -- 0 .U........
      -- 1 .........R
      -- 2 ..........
      -- 3 ..........
      -- 4 ..........
      -- 5 ..........
      -- 6 ..........
      -- 7 ..........
      -- 8 L.........
      -- 9 ........D.
      mapPosition (Point2D 0 1) U L cubeFaceSize `shouldBe`
        Point2D (cubeFaceSize - 2) 0
      mapPosition (Point2D 0 1) U U cubeFaceSize `shouldBe` Point2D 0 1
      mapPosition (Point2D 0 1) U R cubeFaceSize `shouldBe`
        Point2D 1 (cubeFaceSize - 1)
      mapPosition (Point2D 0 1) U D cubeFaceSize `shouldBe`
        Point2D (cubeFaceSize - 1) (cubeFaceSize - 2)

    it "crosses a face border into the neighbouring face" $ do
      crossBorder atlas (Player (Point2D 5 0) L) `shouldBe`
        Player (Point2D 11 14) U
      crossBorder atlas (Player (Point2D 0 11) R) `shouldBe`
        Player (Point2D 11 15) L

    it "loops from a given point" $ do
      let player = Player (Point2D 0 9) R
      let loop = loopFrom atlas player
      let loopSize = 4 * cubeFaceSize atlas
      let expected =
            concat
                -- exits on (3, R)
              [ [Player (Point2D 0 column) R | column <- [9 .. 11]]
                -- reenters on (10, R) and exits on (9, L)
              , [Player (Point2D 11 column) L | column <- [15,14 .. 8]]
                -- reenters on (5, D) and exits on (5, U)
              , [Player (Point2D row 4) U | row <- [7,6 .. 4]]
                -- reenters on (3, L)
              , [Player (Point2D 0 8) R]
              ]

      take loopSize loop `shouldBe` expected
      take (2 * loopSize) loop `shouldBe` expected ++ expected

    it "applies a step" $ do
      let initialPlayer = buildInitialPlayer startingPosition
      let buildPlayer (i, j, _facing) = Player (Point2D i j) _facing
      let expectedSteps = initialPlayer : map buildPlayer
            [ (0, 10, R)
            , (0, 10, D)
            , (5, 10, D)
            , (5, 10, R)
            , (10, 14, D)
            , (10, 14, L)
            , (10, 10, L)
            , (10, 10, D)
            , (5, 1, U)
            , (5, 1, R)
            , (5, 6, R)
            , (5, 6, U)
            , (4, 6, U)
            ]

      scanl (applyStep atlas) initialPlayer path `shouldBe` expectedSteps

    it "applies a path" $ do
      applyPath atlas (buildInitialPlayer startingPosition) path `shouldBe`
        Player (Point2D 4 6) U
