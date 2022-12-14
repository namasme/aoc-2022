module Day10.SecondSpec (spec) where

import Day10.Second
import Test.Hspec

spec :: Spec
spec = do
  instructions <- parseInput <$> runIO (readFile "data/Day10/testInput")

  describe "Solution" $ do
    it "determines if the sprite is over a certain pixel" $ do
      isSpriteOverPixel 5 1 `shouldBe` False
      isSpriteOverPixel 5 4 `shouldBe` True
      isSpriteOverPixel 5 5 `shouldBe` True
      isSpriteOverPixel 5 6 `shouldBe` True
      isSpriteOverPixel 5 10 `shouldBe` False

    it "expands values from a record of events" $ do
      let expectedPrefix = [1, 1, 2, 3, 3, 3, 4]
      let actualValues = expandValues [(1, 1), (3, 2), (4, 3), (7, 4)]

      zipWith (==) expectedPrefix actualValues `shouldSatisfy` and

    it "computes pixels" $ do
      let expectedPrefix = [True, True, False, False, True, True, False, False, True, True]
      let actualPixels = computePixels (CRTSpecs 40 1) instructions

      zipWith (==) expectedPrefix actualPixels `shouldSatisfy` and

    it "shows a CRT from the pixels" $ do
      let crtSpecs = CRTSpecs 40 6
      let pixels = computePixels crtSpecs instructions
      let expected =
            (unlines
               [ "##..##..##..##..##..##..##..##..##..##.."
               , "###...###...###...###...###...###...###."
               , "####....####....####....####....####...."
               , "#####.....#####.....#####.....#####....."
               , "######......######......######......####"
               , "#######.......#######.......#######....."
               ])

      showCRT crtSpecs pixels `shouldBe` init expected -- get rid of the trailing newline from unlines
