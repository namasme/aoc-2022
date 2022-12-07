module Day3.FirstSpec (spec) where

import Day3.First
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO $ readFile "data/Day3/testInput"
  let rucksacks = parseInput testInput

  describe "Parsing" $ do
    it "parses without errors" $ do
      parseInput "vJrwpWtwJgWrhcsFMMfFFhFp" `shouldBe` [("vJrwpWtwJgWr", "hcsFMMfFFhFp")]
      length rucksacks `shouldBe` 6

  describe "Solution" $ do
    it "finds common items" $ do
      findCommonItem ("vJrwpWtwJgWr", "hcsFMMfFFhFp") `shouldBe` 'p'

    it "calculates the priority of a given item" $ do
      priority 'p' `shouldBe` 16
      priority 'L' `shouldBe` 38

    it "calculates the total priority sum" $ do
      (sum . map (priority . findCommonItem) $ rucksacks) `shouldBe` 157
