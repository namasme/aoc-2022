module Day8.SecondSpec (spec) where

import Day8.Second
import Test.Hspec

spec :: Spec
spec = do
  grid <- parseInput <$> runIO (readFile "data/Day8/testInput")

  describe "Solution" $ do
    it "calculates the viewing distance of each position" $ do
      rollingViewingDistance (grid !! 0) `shouldBe` [2, 1, 1, 1, 0]
      rollingViewingDistance (grid !! 1) `shouldBe` [1, 1, 2, 1, 0]
      rollingViewingDistance (grid !! 2) `shouldBe` [4, 3, 1, 1, 0]

    it "calculates the viewing distance of each position starting from the right" $ do
      reverseRollingViewingDistance (grid !! 0) `shouldBe` [0, 1, 2, 3, 1]
      reverseRollingViewingDistance (grid !! 1) `shouldBe` [0, 1, 1, 1, 2]
      reverseRollingViewingDistance (grid !! 2) `shouldBe` [0, 1, 1, 1, 1]

    it "knows how to apply Hadamard product" $ do
      let grid1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
      let grid2 = [[8, 9, 7], [5, 6, 4], [2, 3, 1]]

      hadamardMap (*) grid1 grid2 `shouldBe` [[8, 18, 21], [20, 30, 24], [14, 24, 9]]

    it "calculates the scenic score of every tree" $ do
      scenicScore grid `shouldBe`
        [ [0, 0, 0, 0, 0]
        , [0, 1, 4, 1, 0]
        , [0, 6, 1, 2, 0]
        , [0, 1, 8, 3, 0]
        , [0, 0, 0, 0, 0]
        ]

    it "find the maximum scenic score of all trees" $ do
      maxScenicScore grid `shouldBe` 8
