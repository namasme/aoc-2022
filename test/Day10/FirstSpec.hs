module Day10.FirstSpec (spec) where

import Day10.First
import Test.Hspec

spec :: Spec
spec = do
  instructions <- parseInput <$> runIO (readFile "data/Day10/testInput")

  describe "Parsing" $ do
    it "can parse a single instruction" $ do
      parseInstruction "noop" `shouldBe` Noop
      parseInstruction "addx 123" `shouldBe` Add 123

    it "can parse a list of instructions" $ do
      length instructions `shouldBe` 146
      head instructions `shouldBe` Add 15
      last instructions `shouldBe` Noop

  describe "Solution" $ do
    let events = recordEvents instructions

    it "records events (register changes value) as they happen" $ do
      length events `shouldBe` 1 + length instructions
      head events `shouldBe` (1, 1)
      events !! 1 `shouldBe` (3, 16)
      last events `shouldBe` (241, 17)

    it "can find the register value at any given time" $ do
      let getValueAt t = fst (findValueAtTime t events)
      getValueAt 1 `shouldBe` 1
      getValueAt 2 `shouldBe` 1
      getValueAt 3 `shouldBe` 16
      getValueAt 20 `shouldBe` 21

    it "can find the register value at the specified times" $ do
      findValuesAtTimes [20,60 .. 220] events `shouldBe`
        [21, 19, 18, 21,16, 18]

    it "computes the signal strengths at the specified times" $ do
      computeSignalStrengths [20,60 .. 220] instructions `shouldBe`
        [420, 1140, 1800, 2940, 2880, 3960]
