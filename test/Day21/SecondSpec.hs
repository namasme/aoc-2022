module Day21.SecondSpec (spec) where

import Day21.First.Internal (parseInput)
import Day21.Second
import Test.Hspec

spec :: Spec
spec = do
  monkeys <- parseInput <$> runIO (readFile "data/Day21/testInput")

  describe "Solution" $ do
    describe "Rational functions" $ do
      let f = RationalFunction 1 2 3 4
      let g = RationalFunction 0 5 0 6

      it "adds rational functions" $ do
        add f g `shouldBe` RationalFunction 21 32 18 24

      it "subtracts rational functions" $ do
        sub g f `shouldBe` RationalFunction 9 8 18 24

      it "multiplies rational functions" $ do
        multiply f g `shouldBe` RationalFunction 5 10 18 24

      it "divides rational functions" $ do
        divide f (RationalFunction 0 5 0 1) `shouldBe` RationalFunction 1 2 15 20
        divide (RationalFunction 0 5 0 1) f `shouldBe` RationalFunction 15 20 1 2

      it "reduces rational functions by cancelling out the gcd of the coefficients" $ do
        reduce f `shouldBe` f
        reduce (RationalFunction 3 6 9 12) `shouldBe` f

    it "solves equations" $ do
      solveEquation (RationalFunction 1 2 3 4) (RationalFunction 0 1 0 4) `shouldBe` (-4)

    it "calculates the number you need to yell" $ do
      answer monkeys `shouldBe` 301
