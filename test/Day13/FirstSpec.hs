module Day13.FirstSpec (spec) where

import Day13.First.Internal
import Day13.First
import Test.Hspec

spec :: Spec
spec = do
  packetPairs <- parseInput <$> runIO (readFile "data/Day13/testInput")

  describe "Parsing" $ do
    it "parses without errors" $ do
      length packetPairs `shouldBe` 8
      fst (last packetPairs) `shouldBe`
        [ Number 1
        , List
            [ Number 2
            , List
                [Number 3, List [Number 4, List [Number 5, Number 6, Number 7]]]
            ]
        , Number 8
        , Number 9
        ]

  describe "Solution" $ do
    it "can compare packet values" $ do
      Number 1 < Number 2 `shouldBe` True
      List [Number 1, Number 2] < List [Number 1, Number 3] `shouldBe` True
      List [Number 1] < List [Number 1, Number 0] `shouldBe` True
      compare (List [Number 1, Number 0]) (List [Number 1, Number 0]) `shouldBe` EQ
      compare (List [Number 1]) (Number 1) `shouldBe` EQ
      List [] < Number 0 `shouldBe` True

    it "calculates the solution" $ do
      calculateSolution packetPairs `shouldBe` 13
