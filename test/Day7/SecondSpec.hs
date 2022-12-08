module Day7.SecondSpec (spec) where

import Day7.Second
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day7/testInput")
  let root = parseInput testInput

  describe "Solution" $ do
    it "solves the problem" $ do
      let totalAvailableSpace = 70000000
      let requiredSpace = 30000000

      findOptimalDirectorySize totalAvailableSpace requiredSpace root `shouldBe` 24933642
