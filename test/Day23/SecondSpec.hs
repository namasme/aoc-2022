module Day23.SecondSpec (spec) where

import Day23.First (parseInput)
import Day23.Second
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day23/testInput")
  let elves = parseInput testInput

  describe "Solution" $ do
    it "finds the fixed point" $ do
      stepsUntilFixedPoint elves `shouldBe` 20
