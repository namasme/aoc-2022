module Day23.FirstSpec (spec) where

import qualified Data.Set as S
import Utils.Common (iterateN)
import Utils.Spatial (Direction(..), Point2D(..))
import Day23.First
import Test.Hspec
import Day16.First (encodeValves)
import Day23.First (proposeDestination, Elves (directionSuggestions), attemptMove, calculateEmptyTiles)

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day23/testInput")
  let elves = parseInput testInput

  describe "Parsing" $ do
    it "parses without errors" $ do
      S.size (positions elves) `shouldBe` 22
      S.member (Point2D 0 4) (positions elves) `shouldBe` True
      S.member (Point2D 5 6) (positions elves) `shouldBe` True
      S.member (Point2D 0 0) (positions elves) `shouldBe` False

  describe "Solution" $ do
    -- The coordinate system for the maze is not the one move expects, so the
    -- directions have to be adjusted accordingly.
    -- | Intended | Actual | Delta   |
    -- |----------|--------|---------|
    -- | L        | D      | (0, -1) |
    -- | U        | L      | (-1, 0) |
    -- | R        | U      | (0, 1)  |
    -- | D        | R      | (1, 0)  |

    it "finds the neighbours of a point in a given direction" $ do
      neighbours D (Point2D 1 2) `shouldBe` [Point2D 1 1, Point2D 0 1, Point2D 2 1]
      neighbours R (Point2D 1 2) `shouldBe` [Point2D 2 2, Point2D 2 1, Point2D 2 3]

    it "attempts to move an elf in a direction" $ do
      attemptMove elves L (Point2D 1 2) `shouldBe` Just (Point2D 0 2)
      attemptMove elves U (Point2D 1 2) `shouldBe` Nothing

    it "proposes a valid destination for the given elf" $ do
      -- Isolated elf
      proposeDestination elves (Point2D 0 0) `shouldBe` Point2D 0 0
      -- North move available
      proposeDestination elves (Point2D 0 4) `shouldBe` Point2D (-1) 4
      -- North move unavailable, but East available
      proposeDestination elves (Point2D 1 4) `shouldBe` Point2D 1 5
      -- No move available, stay in position
      proposeDestination elves (Point2D 1 3) `shouldBe` Point2D 1 3

    it "calculates the bounding rectangle for a given configuration of elves" $ do
      let _positions = S.toList (positions elves)

      boundingRectangle _positions `shouldBe` (Point2D 0 0, Point2D 6 6)

    it "simulates one step from each elf" $ do
      let resultElves = applyStep elves

      directionSuggestions resultElves `shouldStartWith`
        (take 8 . tail . directionSuggestions) elves
      S.size (positions resultElves) `shouldBe` S.size (positions elves)
      S.member (Point2D (-1) 4) (positions resultElves) `shouldBe` True
      S.member (Point2D 0 4) (positions resultElves) `shouldBe` False
      S.member (Point2D 7 4) (positions resultElves) `shouldBe` True
      S.member (Point2D 6 4) (positions resultElves) `shouldBe` False
      boundingRectangle (S.toList (positions resultElves)) `shouldBe`
        (Point2D (-1) (-1), Point2D 7 7)

    it "simulates many steps from each elf" $ do
      let resultElves = iterateN 10 applyStep elves

      S.size (positions resultElves) `shouldBe` S.size (positions elves)
      boundingRectangle (S.toList (positions resultElves)) `shouldBe`
        (Point2D (-2) (-2), Point2D 8 9)

    it "calculates the number of empty tiles" $ do
      let resultElves = iterateN 10 applyStep elves

      calculateEmptyTiles resultElves `shouldBe` 110
