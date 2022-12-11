module Utils.SpatialSpec (spec) where

import Utils.Spatial (Direction(..), Point2D(..), move, squaredModulus)
import Test.Hspec

spec :: Spec
spec = do
  describe "Point2D" $ do
    describe "Num instance" $ do
      it "+" $ do
        (Point2D 1 2) + (Point2D 3 4) `shouldBe` Point2D 4 6
      it "-" $ do
        (Point2D 1 2) - (Point2D 3 4) `shouldBe` Point2D (-2) (-2)
      it "*" $ do
        (Point2D 1 2) * (Point2D 3 0) `shouldBe` Point2D 3 0
      it "abs" $ do
       abs (Point2D (-3) 4) `shouldBe` Point2D 3 4
      it "signum" $ do
        signum (Point2D (-3) 0) `shouldBe` Point2D (-1) 0
      it "fromInteger" $ do
       fromInteger 3 `shouldBe` Point2D 3 3

    it "squared modulus" $ do
      squaredModulus (Point2D 1 1) `shouldBe` 2
      squaredModulus (Point2D 3 4) `shouldBe` 25

    it "move" $ do
      move (Point2D 1 1) L `shouldBe` Point2D 0 1
      move (Point2D 1 1) U `shouldBe` Point2D 1 2
      move (Point2D 1 1) R `shouldBe` Point2D 2 1
      move (Point2D 1 1) D `shouldBe` Point2D 1 0
