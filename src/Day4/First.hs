module Day4.First where

import Utils.Common (both, splitOn)

data Interval =
  Interval
    { low :: Int
    , high :: Int
    }
  deriving (Eq, Show)

parseInput :: String -> [(Interval, Interval)]
parseInput = map (both parseInterval . splitOn ',') . lines

parseInterval :: String -> Interval
parseInterval interval = Interval (read _low) (read _high)
  where
    (_low, _high) = splitOn '-' interval

contains :: Interval -> Interval -> Bool
contains (Interval low high) (Interval low' high') = low <= low' && high >= high'

eitherContains :: Interval -> Interval -> Bool
eitherContains i i' = contains i i' || contains i' i

findAllContainingIntervalPairs :: [(Interval, Interval)] -> [(Interval, Interval)]
findAllContainingIntervalPairs = filter (uncurry eitherContains)

solution :: IO ()
solution = do
  intervalPairs <- parseInput <$> readFile "data/Day4/input"

  let fullyContainedCount = length (findAllContainingIntervalPairs intervalPairs)

  print fullyContainedCount
