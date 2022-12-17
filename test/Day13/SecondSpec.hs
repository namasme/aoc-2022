module Day13.SecondSpec (spec) where

import Day13.Second
import Test.Hspec

spec :: Spec
spec = do
  packetPairs <- parseInput <$> runIO (readFile "data/Day13/testInput")

  describe "Solution" $ do
    it "flattens the list of pairs" $ do
      length (flattenPairs packetPairs) `shouldBe` 16
      flattenPairs packetPairs `shouldStartWith` [fst (head packetPairs)]

    it "can offset a list of indices" $ do
      offsetIndices [5, 2, 3, 1] `shouldBe` [8, 3, 5, 1]

    it "finds the positions a list of candidates would fall into were they inserted in order in a given list" $ do
      findSortedPositions [5] [1, 2, 3, 4, 6, 7] `shouldBe` [5]
      findSortedPositions [5, 2] [1, 3, 4, 6, 7] `shouldBe` [5, 2]

    it "calculates the decoder key" $ do
      decoderKey dividerPackets packetPairs `shouldBe` 140
