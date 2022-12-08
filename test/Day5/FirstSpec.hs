module Day5.FirstSpec (spec) where

import Day5.First
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day5/testInput")
  let (stacks, instructions) = parseInput testInput

  describe "Parsing" $ do
    it "parses stacks" $ do
      length stacks `shouldBe` 3
      stacks `shouldBe` ["NZ", "DCM", "P"]

    it "parses instructions" $ do
      length instructions `shouldBe` 4
      head instructions `shouldBe` Instruction 1 2 1

  describe "Solution" $ do
    it "can apply a single instruction" $ do
      applyInstruction stacks (head instructions) `shouldBe` ["DNZ", "CM", "P"]

    it "can apply multiple instructions" $ do
      applyInstructions stacks instructions `shouldBe` ["C", "M", "ZNDP"]

    it "gets the head of each stack" $ do
      getHeads (applyInstructions stacks instructions) `shouldBe` "CMZ"
