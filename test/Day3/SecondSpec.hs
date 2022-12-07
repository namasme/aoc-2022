module Day3.SecondSpec (spec) where

import Day3.Second
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO $ readFile "data/Day3/testInput"
  let elfGroups = parseInput testInput

  describe "Parsing" $ do
    it "parses without errors" $ do
      length elfGroups `shouldBe` 2
      map length elfGroups `shouldSatisfy` all (==3)

  describe "Solution" $ do
    it "finds the badge for a given elf group" $ do
      findBadge (head elfGroups) `shouldBe` 'r'
      findBadge (elfGroups !! 1) `shouldBe` 'Z'

    it "calculates the total priority sum" $ do
      (sum . map (priority . findBadge) $ elfGroups) `shouldBe` 70
