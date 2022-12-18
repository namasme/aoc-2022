module Day11.SecondSpec (spec) where

import Control.Monad.Writer
import qualified Data.Map as M
import Utils.Common (iterateM)

import Day11.Second
import Test.Hspec

spec :: Spec
spec = do
  troop <- parseInput <$> runIO (readFile "data/Day11/testInput")

  describe "Solution" $ do
    it "collects the moduli of all monkeys" $ do
      collectModuli troop `shouldBe` [23, 19, 13, 17]

    it "can simulate 10000 rounds" $ do
      let moduli = collectModuli troop
      let finalRound = iterateM (monkeyRound (itemReducer moduli)) (return troop) !! 10000
      let throwsCounter = execWriter finalRound
      let expectedThrowsCounter = M.fromList [(0, 52166), (1, 47830), (2, 1938), (3, 52013)]

      counter throwsCounter `shouldBe` expectedThrowsCounter
      monkeyBusiness throwsCounter `shouldBe` 2713310158
