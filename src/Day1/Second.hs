module Day1.Second
  ( module Day1.Second
  , parseInput
  ) where

import Data.List (sort)
import Day1.First (parseInput, totalCalories)

findTopThreeMostCaloriesElves :: [[Int]] -> Int
findTopThreeMostCaloriesElves = sum . take 3 . reverse . sort . map totalCalories

solution :: IO ()
solution = do
  parsedInput <- parseInput <$> readFile "data/Day1/input"

  print (findTopThreeMostCaloriesElves parsedInput)
