module Day2.FirstSpec (spec) where

import Day2.First
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO $ readFile "data/Day2/testInput"
  let parsedInput = parseInput testInput

  describe "Parsing" $ do
    it "parses without errors" $ do
      parsedInput `shouldBe` [('A', 'Y'), ('B', 'X'), ('C', 'Z')]

  describe "Solution" $ do
    it "solves the problem" $ do
      let matches = map parseMatch parsedInput

      totalScores (map (uncurry scores) matches) `shouldBe` (15, 15)
