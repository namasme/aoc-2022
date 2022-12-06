module Day{N}.First where

solution :: IO ()
solution = do
  parsedInput <- parseInput <$> readFile "data/Day{N}/input"

  print (parsedInput)
