module Day17.SecondSpec (spec) where

import Day17.Second
import Utils.Common (iterateN)
import Test.Hspec

spec :: Spec
spec = do
  jetsSeed <- parseInput <$> runIO (readFile "data/Day17/testInput")
  let initialChamber = buildChamber jetsSeed

  describe "Solution" $ do
    it "snapshots chambers" $ do
      snapshotChamber initialChamber `shouldBe` (0, 0)

      let after3 = iterateN 3 dropShape initialChamber
      snapshotChamber after3 `shouldBe` (3, 6)

    it "keys chambers" $ do
      keyChamber initialChamber `shouldBe` (0, 0)

      let after3 = iterateN 3 dropShape initialChamber
      keyChamber after3 `shouldBe` (3, 13)

    it "calculates final height" $ do
      heightAfter initialChamber 1000000000000 `shouldBe` 1514285714288
