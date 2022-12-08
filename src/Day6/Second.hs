module Day6.Second
  ( module Day6.Second
  , findStartOfPacketMarker
  ) where

import Day6.First (findStartOfPacketMarker)

solution :: IO ()
solution = do
  stream <- readFile "data/Day6/input"

  print (findStartOfPacketMarker 14 stream)
