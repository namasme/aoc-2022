module Day13.First
  ( module Day13.First
  , parseInput
  , PacketPair
  , Packet
  , PacketValue(..)
  ) where

import Day13.First.Internal (Packet, PacketPair, PacketValue(..), parseInput)

instance Ord PacketValue where
  compare (List xs) (List ys) = compare xs ys
  compare (List xs) y = compare xs [y]
  compare x (List ys) = compare [x] ys
  compare (Number x) (Number y) = compare x y

calculateSolution :: [PacketPair] -> Int
calculateSolution = sum . map fst . filter (uncurry (<=) . snd) . zip [1 ..]

solution :: IO ()
solution = do
  packetPairs <- parseInput <$> readFile "data/Day13/input"

  print (calculateSolution packetPairs)
