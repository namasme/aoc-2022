module Day5.SecondSpec (spec) where

import Day5.Second
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day5/testInput")
  let (stacks, instructions) = parseInput testInput

  describe "Solution" $ do
    it "can apply a single instruction" $ do
      applyInstruction ["DNZ", "CM", "P"] (instructions !! 1) `shouldBe` ["", "CM", "DNZP"]

    it "can apply multiple instructions" $ do
      applyInstructions stacks instructions `shouldBe` ["M", "C", "DNZP"]
