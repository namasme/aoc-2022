module Day19.SecondSpec (spec) where

import Day19.Second
import Test.Hspec

spec :: Spec
spec = do
  blueprints <- parseInput <$> runIO (readFile "data/Day19/testInput")
  let blueprint = head blueprints


  describe "Solution" $ do
    it "calculates a lower bound for how much geode a state will yield" $ do
      minimumExpectedGeode 30 initialState `shouldBe` 0
      minimumExpectedGeode 10 initialState { robots = [0, 0, 0, 10] } `shouldBe` 100

    it "calculates an upper bound for how much geode a state will yield" $ do
      maximumExpectedGeode 0 initialState `shouldBe` 0
      maximumExpectedGeode 2 initialState `shouldBe` 1
      maximumExpectedGeode 11 initialState `shouldBe` 55

    it "determines if a state is worth continuing to explore" $ do
      initialState `shouldSatisfy` isWorth 11 54
      initialState `shouldNotSatisfy` isWorth 11 55

    it "calculates the maximum amount of geode that can be produced with a given blueprint" $ do
      findMaximumGeode 32 initialState blueprint `shouldBe` 56

    it "calculates the final answer" $ do
      calculateAnswer 32 blueprints `shouldBe` 56 * 62
