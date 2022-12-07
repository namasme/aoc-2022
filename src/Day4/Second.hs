module Day4.Second
  ( module Day4.Second
  , parseInput
  , Interval(..)
  ) where

import Day4.First (Interval(..), parseInput)

overlap :: Interval -> Interval -> Bool
overlap (Interval low high) (Interval low' high') =
  (high >= low' && low <= high') || (high' >= low && low' <= high)

findAllOverlappingIntervalPairs :: [(Interval, Interval)] -> [(Interval, Interval)]
findAllOverlappingIntervalPairs = filter (uncurry overlap)

solution :: IO ()
solution = do
  intervalPairs <- parseInput <$> readFile "data/Day4/input"

  let overlappingCount = length (findAllOverlappingIntervalPairs intervalPairs)

  print overlappingCount
