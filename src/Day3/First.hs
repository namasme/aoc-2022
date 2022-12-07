module Day3.First where

import Data.Char (isLower, ord)
import qualified Data.Set as S

type Item = Char
type Compartment = String
type Rucksack = (Compartment, Compartment)

parseInput :: String -> [Rucksack]
parseInput = map half . lines
  where
    half s = splitAt (length s `div` 2) s

findCommonItem :: Rucksack -> Item
findCommonItem (c1, c2) = head . S.toList $ S.intersection (S.fromList c1) (S.fromList c2)

priority :: Item -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

solution :: IO ()
solution = do
  rucksacks <- parseInput <$> readFile "data/Day3/input"

  let commonItemPriorities = map (priority . findCommonItem) rucksacks

  print (sum commonItemPriorities)
