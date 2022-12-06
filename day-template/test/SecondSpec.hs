module Day{N}.SecondSpec (spec) where

import Day{N}.Second
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day{N}/testInput")

  describe "Parsing" $ do
    it "parses without errors" $ do
      0 `shouldBe` 0

  describe "Solution" $ do
    it "solves the problem" $ do
      1 `shouldBe` 1
