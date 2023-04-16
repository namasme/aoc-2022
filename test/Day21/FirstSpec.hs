module Day21.FirstSpec (spec) where

import Day21.First.Internal
import Day21.First
import Test.Hspec

spec :: Spec
spec = do
  monkeys <- parseInput <$> runIO (readFile "data/Day21/testInput")

  describe "Parsing" $ do
    it "parses the input" $ do
      length monkeys `shouldBe` 15
      head monkeys `shouldBe` ("root", Add "pppw" "sjmn")
      monkeys !! 1 `shouldBe` ("dbpl", Literal 5)

  describe "Solution" $ do
    it "applies a job" $ do
      apply (Literal 0) `shouldBe` 0
      apply (Add 1 2) `shouldBe` 3
      apply (Division 9 3) `shouldBe` 3

    it "applies nested jobs" $ do
      let iso n = (Iso (Literal n) :: Fix Job)
      let jobTree = Iso . fmap Iso $ Add (Product (iso 2) (iso 3)) (Subtract (iso 4) (iso 1))
      cata apply jobTree `shouldBe` (2 * 3) + (4 - 1)

    it "calculates what root yells" $ do
      answer monkeys `shouldBe` 152
