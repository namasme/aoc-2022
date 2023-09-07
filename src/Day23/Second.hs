module Day23.Second (module Day23.Second) where

import Day23.First (Elves(..), applyStep, parseInput)

stepsUntilFixedPoint :: Elves -> Int
stepsUntilFixedPoint elves = stepsUntilFixedPointAux 1 elves (applyStep elves)

stepsUntilFixedPointAux :: Int -> Elves -> Elves -> Int
stepsUntilFixedPointAux counter previous current
  | positions previous == positions current = counter
  | otherwise = stepsUntilFixedPointAux (counter + 1) current (applyStep current)

solution :: IO ()
solution = do
  elves <- parseInput <$> readFile "data/Day23/input"

  print (stepsUntilFixedPoint elves)
