module Day6.SecondSpec (spec) where

import Day6.Second
import Test.Hspec

spec :: Spec
spec = do
  describe "Solution" $ do
    it "solves the problem" $ do
      let solve = findStartOfPacketMarker 14
      solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 19
      solve "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23
      solve "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 23
      solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 29
      solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 26
