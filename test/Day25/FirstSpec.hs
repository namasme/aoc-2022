module Day25.FirstSpec (spec) where

import Day25.First
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day25/testInput")
  let numerals = parseInput testInput

  describe "Parsing" $ do
    it "parses the numerals" $ do
      length numerals `shouldBe` 13
      head numerals `shouldBe` [One, DoubleMinus, Minus, Zero, Minus, Two]
      last numerals `shouldBe` [One, Two, Two]

  describe "Solution" $ do
    let decimalNumerals = [1747, 906, 198, 11, 201, 31, 1257, 32, 353, 107, 7, 3, 37]

    it "translates from SNAFU to decimal" $ do
      map fromSNAFU numerals `shouldBe` decimalNumerals

    it "translates from decimal to SNAFU" $ do
      map toSNAFU decimalNumerals `shouldBe` numerals

    it "represents SNAFU numerals as strings" $ do
      showSNAFUNumeral [DoubleMinus, Minus, Zero, One, Two] `shouldBe` "=-012"

    it "calculates the answer for Bob" $ do
      showSNAFUNumeral (findAnswer numerals) `shouldBe` "2=-1=0"
