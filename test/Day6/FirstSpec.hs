module Day6.FirstSpec (spec) where

import qualified Data.Map as M
import Day6.First
import Test.Hspec

spec :: Spec
spec = do
  describe "Counters" $ do
    it "knows how to add a new element to a given counter" $ do
      addElem (M.fromList [('b', 3)]) 'a' `shouldBe` M.fromList [('a', 1), ('b', 3)]

    it "knows how to add an existing element to a given counter" $ do
      addElem (M.fromList [('a', 3)]) 'a' `shouldBe` M.fromList [('a', 4)]

    it "buils a counter from a given list" $ do
      fromList "abaccda" `shouldBe`
        M.fromList [('a', 3), ('b', 1), ('c', 2), ('d',1)]

  describe "Solution" $ do
    it "solves the problem" $ do
      let solve = findStartOfPacketMarker 4
      solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 7
      solve "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
      solve "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 6
      solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
      solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 11
