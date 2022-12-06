module Day{N}.Second (module Day{N}.Second) where

import Day{N}.First

solution :: IO ()
solution = do
  parsedInput <- parseInput <$> readFile "data/Day{N}/input"

  print (parsedInput)
