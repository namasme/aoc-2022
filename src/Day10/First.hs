module Day10.First where

import Utils.Common (splitOn)

type Time = Int
data Instruction
  = Add Int
  | Noop
  deriving (Eq, Show)

parseInput :: String -> [Instruction]
parseInput = map parseInstruction . lines

computeSignalStrengths :: [Time] -> [Instruction] -> [Int]
computeSignalStrengths times instructions = zipWith (*) times (findValuesAtTimes times events)
  where
    events = recordEvents instructions

findValuesAtTimes :: [Time] -> [(Time, Int)] -> [Int]
findValuesAtTimes [] _ = []
findValuesAtTimes (t:times) events = value : findValuesAtTimes times restEvents
  where
    (value, restEvents) = findValueAtTime t events

findValueAtTime :: Time -> [(Time, Int)] -> (Int, [(Time, Int)])
findValueAtTime time (e1:e2:events)
  | fst e2 > time = (snd e1, e2:events)
  | otherwise = findValueAtTime time (e2:events)
findValueAtTime _ _ = error "Unexpected"

recordEvents :: [Instruction] -> [(Time, Int)]
recordEvents instructions = zip times values
  where
    times = scanl (+) 1 (map toTime instructions)
    values = scanl (+) 1 (map toValue instructions)

toTime :: Instruction -> Time
toTime Noop = 1
toTime (Add _) = 2

toValue :: Instruction -> Int
toValue Noop = 0
toValue (Add x) = x

parseInstruction :: String -> Instruction
parseInstruction "noop" = Noop
parseInstruction line = Add . read . snd . splitOn ' ' $ line

solution :: IO ()
solution = do
  instructions <- parseInput <$> readFile "data/Day10/input"
  let times = [20,60 .. 220]
  let signalStrengths = computeSignalStrengths times instructions

  print (sum signalStrengths)
