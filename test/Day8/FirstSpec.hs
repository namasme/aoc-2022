module Day8.FirstSpec (spec) where

import qualified Data.Set as S
import Day8.First
import Test.Hspec

spec :: Spec
spec = do
  grid <- parseInput <$> runIO (readFile "data/Day8/testInput")

  describe "Parsing" $ do
    it "parses without errors" $ do
      length grid `shouldBe` 5
      map length grid `shouldSatisfy` all (==5)
      head grid `shouldBe` [3, 0, 3, 7, 3]

  describe "Solution" $ do
    it "finds the visible indices when looking from one side" $ do
      orientedVisibleIndices (grid !! 0) `shouldBe` S.fromList [0, 3]
      orientedVisibleIndices (grid !! 1) `shouldBe` S.fromList [0, 1]
      orientedVisibleIndices (grid !! 2) `shouldBe` S.fromList [0]

    it "finds the visible indices when looking from both sides" $ do
      dimensionVisibleIndices (grid !! 0) `shouldBe` S.fromList [0, 3, 4]
      dimensionVisibleIndices (grid !! 1) `shouldBe` S.fromList [0, 1, 2, 4]
      dimensionVisibleIndices (grid !! 2) `shouldBe` S.fromList [0, 1, 3, 4]

    it "finds all the visible trees in the grid" $ do
      let visiblePositions = gridVisibleIndices grid
      let isEdgePosition (x, y) = x == 0 || y == 0 || x == 4 || y == 4

      S.size visiblePositions `shouldBe` 21
      (S.size . S.filter isEdgePosition) visiblePositions `shouldBe` 16
      (S.size . S.filter (not . isEdgePosition)) visiblePositions `shouldBe` 5
