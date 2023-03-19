{-# LANGUAGE BinaryLiterals #-}
module Day17.FirstSpec (spec) where

import Data.Bits ((.|.), shiftL, shiftR)
import Lens.Micro.Platform
import Day17.First
import Test.Hspec

spec :: Spec
spec = do
  jetsSeed <- parseInput <$> runIO (readFile "data/Day17/testInput")
  let initialChamber = buildChamber jetsSeed

  describe "Parsing" $ do
    it "parses a single jet" $ do
      parseJet '<' `shouldBe` L
      parseJet '>' `shouldBe` R

    it "parses without errors" $ do
      length jetsSeed `shouldBe` 40
      jetsSeed `shouldStartWith` [R, R, R, L, L]
      jetsSeed `shouldEndWith` [R, L, L, R, R]

  describe "Solution" $ do
    it "builds a chamber correctly from the jets seed" $ do
      initialChamber ^. rows `shouldBe` []
      take 5 (initialChamber ^. shapes) `shouldBe` [HLine, Cross, LShape, VLine, Square] -- 🎵
      initialChamber ^. shapeIdx `shouldBe` (0, 5)
      take (length jetsSeed) (initialChamber ^. jets) `shouldBe` jetsSeed
      initialChamber ^. jetsIdx `shouldBe` (0, length jetsSeed)
      initialChamber ^. epoch `shouldBe` 0

    describe "Low-level operations" $ do
      it "maps jets to shifts" $ do
        toShift L 6 `shouldBe` 12
        toShift R 6 `shouldBe` 3

      it "increases a cyclic counter" $ do
        next (0, 5) `shouldBe` (1, 5)
        next (1, 5) `shouldBe` (2, 5)
        next (4, 5) `shouldBe` (0, 5)

      it "zips two lists with a function, filling with the suffix of the longest" $ do
        zipWithLongest (+) [1, 2, 3] [4, 5, 6, 7] `shouldBe` [5, 7, 9, 7]

    it "determines when a shape would collide with the walls of the chamber upon being pushed" $ do
      collides R [0b0000001] undefined `shouldBe` True
      collides L [0b1000000] undefined `shouldBe` True

    it "determines whether a shape would collide with the current rocks upon being pushed" $ do
      collides L [0b0000100] [0b0001000] `shouldBe` True
      collides R [0b0000100] [0b0001000] `shouldBe` False
      collides R [0b0000100, 0b0000100] [0b0001000, 0b0000010] `shouldBe` True

    describe "Chamber operations" $ do
      it "finds the current jet" $ do
        currentJet initialChamber `shouldBe` head jetsSeed

      it "updates chamber state to point to the next jet" $ do
        let result = advanceJet initialChamber

        currentJet result `shouldBe` R
        result ^. jetsIdx . _1 `shouldBe` 1

      it "finds the current shape" $ do
        currentShape initialChamber `shouldBe` HLine

      it "updates chamber state to point to the next shape" $ do
        let result = advanceShape initialChamber

        currentShape result `shouldBe` Cross
        result ^. shapeIdx . _1 `shouldBe` 1

    it "when possible, pushes a shape by the provided jet" $ do
      -- #......
      -- #....#.
      -- #...###
      -- #....#.
      let chamberRows = zipWith (.|.) (map (`shiftL` 2) (toRows VLine)) (0:map (`shiftR` 2) (toRows Cross))
      let shapeRows = toRows Square

      -- #.@@...
      -- #.@@.#.
      -- #...###
      -- #....#.
      attemptPush L shapeRows chamberRows `shouldBe` map (toShift L) shapeRows
      attemptPush R shapeRows chamberRows `shouldBe` map (toShift R) shapeRows

      -- #.@@.#.
      -- #.@@###
      -- #....#.
      attemptPush L shapeRows (tail chamberRows) `shouldBe` map (toShift L) shapeRows
      attemptPush R shapeRows (tail chamberRows) `shouldBe` shapeRows

    it "when possible, it lets the shape descend one row; otherwise lands it" $ do
      -- .......
      -- .......
      -- ....#..
      -- ...###.
      -- ....#..
      let chamberRows = [0, 0] ++ map (toShift R) (toRows Cross)
      let shapeRows = toRows Square

      -- ..@@...
      -- ..@@...
      -- ....#..
      -- ...###.
      -- ....#..
      attemptFall shapeRows chamberRows `shouldBe` Nothing

      -- ..@@...
      -- ..@@#..
      -- ...###.
      -- ....#..
      attemptFall shapeRows (tail chamberRows) `shouldBe`
        Just [ 0b0011000
             , 0b0011100
             , 0b0001110
             , 0b0000100
             ]

    it "simulates the fall of the current shape" $ do
      let result = simulateFall initialChamber

      result ^. rows `shouldBe` [0b0011110]
      result ^. jetsIdx . _1 `shouldBe` 4

    it "calculates the height after a number of shapes are dropped" $ do
      heightAfter initialChamber 1 `shouldBe` 1
      heightAfter initialChamber 2 `shouldBe` 4
      heightAfter initialChamber 3 `shouldBe` 6
      heightAfter initialChamber 5 `shouldBe` 9
      heightAfter initialChamber 10 `shouldBe` 17
      heightAfter initialChamber 2022 `shouldBe` 3068
