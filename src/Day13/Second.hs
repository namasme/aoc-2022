module Day13.Second
  ( module Day13.Second
  , parseInput
  , PacketValue(..)
  ) where

import Data.Function (on)
import Data.List (sort, sortBy)

import Day13.First (Packet, PacketPair, PacketValue(..), parseInput)

decoderKey :: [Packet] -> [PacketPair] -> Int
decoderKey dividers = product . findSortedPositions dividers . flattenPairs

dividerPackets :: [Packet]
dividerPackets = [[List [Number 2]], [List [Number 6]]]

findSortedPositions :: Ord a => [a] -> [a] -> [Int]
findSortedPositions candidates = offsetIndices . foldl updateIndices initialIndices
  where
    updateIndices indices a = zipWith (+) indices $ map (fromEnum . (> a)) candidates
    initialIndices = replicate (length candidates) 1

offsetIndices :: [Int] -> [Int]
offsetIndices =
  map snd .
  sortBy (compare `on` fst) .
  zipWith offsetIndex [0 ..] . sortBy (compare `on` snd) . zip [0 ..]
  where
    offsetIndex offset (candidateIdx, outputIdx) =
      (candidateIdx, outputIdx + offset)

flattenPairs :: [PacketPair] -> [Packet]
flattenPairs = concatMap (\(a, b) -> [a, b])

solution :: IO ()
solution = do
  packetPairs <- parseInput <$> readFile "data/Day13/input"

  print (decoderKey dividerPackets packetPairs)
