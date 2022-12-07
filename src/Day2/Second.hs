module Day2.Second
  ( module Day2.Second
  , parseInput
  , Shape(..)
  , shapeScore
  ) where

import Data.Bifunctor (bimap)
import Day2.First (parseInput, Shape(..), shapeScore)

data Result = Win | Draw | Loss

matchScore :: Shape -> Result -> Int
matchScore s r = ceilMod (shapeScore s + offsetForResult r) + resultScore r
  where
    ceilMod x = 3 - ((-x) `mod` 3)

decryptShape :: Char -> Shape
decryptShape 'A' = Rock
decryptShape 'B' = Paper
decryptShape 'C' = Scissors
decryptShape _ = undefined

decryptResult :: Char -> Result
decryptResult 'X' = Loss
decryptResult 'Y' = Draw
decryptResult 'Z' = Win
decryptResult _ = undefined

resultScore :: Result -> Int
resultScore Win = 6
resultScore Draw = 3
resultScore Loss = 0

offsetForResult :: Result -> Int
offsetForResult Win = 1
offsetForResult Draw = 0
offsetForResult Loss = -1

playMatches :: [(Char, Char)] -> [Int]
playMatches = map (uncurry matchScore . bimap decryptShape decryptResult)

solution :: IO ()
solution = do
  matches <- parseInput <$> readFile "data/Day2/input"

  let results = playMatches matches
  let totalOwnScore = sum results

  print totalOwnScore
