module Day20.FirstSpec (spec) where

import qualified Data.Map as M
import qualified Data.Set as S

import Day20.First
import Test.Hspec

spec :: Spec
spec = do
  values <- parseInput <$> runIO (readFile "data/Day20/testInput")
  describe "Parsing" $ do
    it "parses without errors" $ do
      values `shouldBe` [1, 2, -3, 3, -2, 0, 4]

  describe "Solution" $ do
    it "converts to cycle" $ do
      toCycle [1, 2, 3] `shouldBe` OSet { ordered = [1, 2, 3], elements = S.fromList [1, 2, 3]}

    it "builds cycles" $ do
      buildCycle 1 5 `shouldBe` toCycle [5, 4, 3, 2, 1]
      buildCycle 4 0 `shouldBe` toCycle [0, 1, 2, 3, 4]

    it "converts to chunks" $ do
      toChunks [(1, [5, 6, 7]), (2, [4]), (3, [])] `shouldBe`
        [(1, ([5, 6, 7], 2)), (2, ([4], 3)), (3, ([], 1))]

    it "chunkifyAux" $ do
      chunkifyAux (S.fromList [1, 2, 3]) [4] [1, 3, 6, 8, 9, 2] `shouldBe`
        [(1, []), (3, [6, 8, 9]), (2, [4])]

    it "chunkifies a cycle" $ do
      let common = S.fromList [1, 2, 3]

      chunkify common [4, 1, 3, 6, 8, 9, 2] `shouldBe`
        M.fromList [(1, ([], 3)), (3, ([6, 8, 9], 2)), (2, ([4], 1))]

    it "chunkifies with no elements in common" $ do
      chunkify S.empty [4, 1, 3, 6, 8, 9, 2] `shouldBe` M.empty

    it "applies permutation" $ do
      let permutation = [toCycle [1, 3, 5], toCycle [2, 6, 0]]

      apply 0 permutation `shouldBe` 2
      apply 1 permutation `shouldBe` 3
      apply 3 permutation `shouldBe` 5
      apply 4 permutation `shouldBe` 4

    it "applies the inverse of a permutation" $ do
      let permutation = [toCycle [1, 3, 5], toCycle [2, 6, 0]]

      applyInverse 0 permutation `shouldBe` 6
      applyInverse 1 permutation `shouldBe` 5
      applyInverse 3 permutation `shouldBe` 1
      applyInverse 4 permutation `shouldBe` 4

    it "appends a new cycle to the permutation" $ do
      snoc (toCycle [5, 1, 7, 10, 3, 11, 2]) [toCycle [4, 1, 3, 6, 8, 9, 2]] `shouldMatchList`
        [toCycle [1, 11, 2, 4, 7, 10, 3, 6, 8, 9, 5]]
      snoc (toCycle [5, 1, 7, 10, 11, 2]) [toCycle [4, 1, 3, 6, 8, 9, 2]] `shouldMatchList`
        [toCycle [2, 4, 7, 10, 11], toCycle [1, 3, 6, 8, 9, 5]]

    it "appends a new disjoint cycle to the permutation" $ do
      snoc (toCycle [4, 6, 8, 9]) [toCycle [5, 1, 7, 10, 3, 11, 2]] `shouldMatchList`
        [toCycle [4, 6, 8, 9], toCycle [5, 1, 7, 10, 3, 11, 2]]

    it "updates current permutation with new value" $ do
      updatePermutation 7 [] (0, 1) `shouldBe` [toCycle [1, 0]]
      updatePermutation 7 [] (3, 4) `shouldBe` [toCycle [1, 2, 3]]
      updatePermutation 7 [toCycle [0, 1]] (1, 2) `shouldBe` [toCycle [1, 2]]

    it "calculates the final permutation" $ do
      calculatePermutation values `shouldBe` [toCycle [4, 0, 1, 2, 3, 6]]

    it "calculates the grove coordinates" $ do
      findGroveCoordinates values `shouldBe` 3
