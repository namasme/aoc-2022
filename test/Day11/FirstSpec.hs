module Day11.FirstSpec (spec) where

import Control.Monad.Writer
import Data.Map ((!))
import qualified Data.Map as M
import Lens.Micro.Platform
import Utils.Common (iterateM)
import Day11.First.Internal
import Day11.First
import Test.Hspec

spec :: Spec
spec = do
  troop <- parseInput <$> runIO (readFile "data/Day11/testInput")
  let first = troop ! 0

  describe "Parsing" $ do
    it "parses without errors" $ do

      M.size troop `shouldBe` 4
      first ^. items `shouldBe` [79, 98]
      (first ^. inspect) 1 `shouldBe` 19
      first ^. modulus `shouldBe` 23
      first ^. destinationIds `shouldBe` (2, 3)

  describe "Solution" $ do
    let itemReducer = (`div` 3)

    it "calculates where a given monkey will throw each item" $ do
      monkeyThrows itemReducer 0 first `shouldBe` [Throw 0 3 500, Throw 0 3 620]

    it "backfills the rest of monkeys to include the items they haven been thrown" $ do
      let throws = monkeyThrows itemReducer 0 first
      let backfilledTroop = backfillMonkeys troop throws
      let expectedNewItems = map item throws

      (backfilledTroop ! 0) ^. items `shouldBe` first ^. items
      (backfilledTroop ! 3) ^. items `shouldBe` (troop ! 3) ^. items ++ expectedNewItems

    it "can simulate a full round" $ do
      let (finalTroop, throwsCounter) = runWriter (monkeyRound itemReducer troop)
      let expectedItems =
            M.fromList
              [ (0, [20, 23, 27, 26])
              , (1, [2080, 25, 167, 207, 401, 1046])
              , (2, [])
              , (3, [])
              ]
      let expectedThrowsCounter = M.fromList [(0, 2), (1, 4), (2, 3), (3, 5)]

      M.map (^. items) finalTroop `shouldBe` expectedItems
      counter throwsCounter `shouldBe` expectedThrowsCounter

    it "can simulate 20 rounds" $ do
      let finalRound = iterateM (monkeyRound itemReducer) (return troop) !! 20
      let (finalTroop, throwsCounter) = runWriter finalRound
      let expectedItems =
            M.fromList
              [ (0, [10, 12, 14, 26, 34])
              , (1, [245, 93, 53, 199, 115])
              , (2, [])
              , (3, [])
              ]
      let expectedThrowsCounter = M.fromList [(0, 101), (1, 95), (2, 7), (3, 105)]

      M.map (^. items) finalTroop `shouldBe` expectedItems
      counter throwsCounter `shouldBe` expectedThrowsCounter

    it "calculates monkey business" $ do
      let finalRound = iterateM (monkeyRound itemReducer) (return troop) !! 20
      let throwsCounter = execWriter finalRound

      monkeyBusiness throwsCounter `shouldBe` 10605
