module Day6.First where

import qualified Data.Map as M
import Data.List.Split (divvy)
import Lens.Micro.Platform

type Counter a = M.Map a Int

-- This could be optimized, by _windowing_ the counter as well as it moves through the list,
-- instead of building it from scratch at every point. However, given the size of the input
-- and the laziness of map and filter, this is probably overkill.
-- It also assumes a solution *will* be found, but again, it works well enough.
findStartOfPacketMarker :: Int -> String -> Int
findStartOfPacketMarker windowSize =
  fst .
  head . filter (uniqueEntries . snd) . zip [windowSize ..] . map fromList . divvy windowSize 1
  where
    uniqueEntries = all (== 1) . M.elems

fromList :: Ord a => [a] -> Counter a
fromList = foldl addElem M.empty

addElem :: Ord a => Counter a -> a -> Counter a
addElem counter a = counter & at a . non 0 %~ succ

solution :: IO ()
solution = do
  stream <- readFile "data/Day6/input"

  print (findStartOfPacketMarker 4 stream)
