module Day2.First where

import Data.Function (on)
import qualified Data.Map as M
import Data.Map ((!))
import Data.Tuple (swap)
import Utils.Common (both, splitOn)

data Shape = Rock | Paper | Scissors deriving (Eq, Show)
type DecryptionMapping = M.Map Char Shape

parseInput :: String -> [(Char, Char)]
parseInput = map (both head . splitOn ' ') . lines

decrypt :: DecryptionMapping -> Char -> Shape
decrypt = (!)

decryptionMapping =
  M.fromList
    [ ('A', Rock)
    , ('B', Paper)
    , ('C', Scissors)
    , ('X', Rock)
    , ('Y', Paper)
    , ('Z', Scissors)
    ]

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

scores :: Shape -> Shape -> (Int, Int)
scores Rock Paper = (1, 8)
scores Rock Scissors = (7, 3)
scores Paper Scissors = (2, 9)
scores x y
  | x == y = (3 + shapeScore x, 3 + shapeScore x)
  | otherwise = swap (scores y x)

addScores :: (Int, Int) -> (Int, Int) -> (Int, Int)
addScores (x, y) (x', y') = (x + x', y + y')

totalScores :: [(Int, Int)] -> (Int, Int)
totalScores = foldl addScores (0, 0)

parseMatch :: (Char, Char) -> (Shape, Shape)
parseMatch = both (decrypt decryptionMapping)

solution :: IO ()
solution = do
  rawMatches <- parseInput <$> readFile "data/Day2/input"

  let matches = map parseMatch rawMatches
  let results = map (uncurry scores) matches
  let totalOwnScore = snd (totalScores results)

  print totalOwnScore
