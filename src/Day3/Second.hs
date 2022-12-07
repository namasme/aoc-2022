module Day3.Second
  ( module Day3.Second
  , priority
  ) where

import qualified Data.Set as S

import Data.List.Split (chunksOf)

import Day3.First (Item, priority)

type ElfGroup = [String]

parseInput :: String -> [ElfGroup]
parseInput = chunksOf 3 . lines

findBadge :: ElfGroup -> Item
findBadge = head . S.toList . foldl1 S.intersection . map S.fromList

solution :: IO ()
solution = do
  elfGroups <- parseInput <$> readFile "data/Day3/input"

  let badgesPriorities = map (priority . findBadge) elfGroups

  print (sum badgesPriorities)
