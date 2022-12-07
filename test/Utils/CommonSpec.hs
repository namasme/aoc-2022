module Utils.CommonSpec (spec) where

import Utils.Common
import Test.Hspec

spec :: Spec
spec = do
  describe "Splitting" $ do
    it "splitWhen" $ do
      splitWhen (> 3) [3, 1, 2, 4, 1, 3] `shouldBe` ([3, 1, 2], [1, 3])

    it "splitOn" $ do
      splitOn ' ' "foo bar" `shouldBe` ("foo", "bar")

  describe "Other utilities" $ do
    it "both applies transformation to both members of a tuple" $ do
      both (10-) (3, 5) `shouldBe` (7, 5)
