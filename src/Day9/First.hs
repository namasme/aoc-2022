module Day9.First where

import qualified Data.Set as S

import Utils.Common (splitOn)
import Utils.Spatial (Direction(..), Point2D(..), move, squaredModulus)

data Motion =
  Motion
    { direction :: Direction
    , steps :: Int
    }
  deriving (Eq, Show)
data Rope =
  Rope
    { start :: Point2D
    , end :: Point2D
    }
  deriving (Eq, Show)

parseInput :: String -> [Motion]
parseInput = map parseMotion . lines

initialRope = Rope (Point2D 0 0) (Point2D 0 0)

findTailVisitedPositions :: [Rope] -> S.Set Point2D
findTailVisitedPositions = S.fromList . map end

trackMotions :: [Motion] -> [Rope]
trackMotions motions = scanl applyStep initialRope steps
  where
    steps = concatMap toSteps motions

applyMotion :: Rope -> Motion -> Rope
applyMotion rope motion = foldl applyStep rope (toSteps motion)

applyStep :: Rope -> Direction -> Rope
applyStep rope direction = Rope newStart newEnd
  where
    newStart = move (start rope) direction
    newEnd = updateEnd newStart (end rope)

updateEnd :: Point2D -> Point2D -> Point2D
updateEnd newStart oldEnd
  | isRopeValid newStart oldEnd = oldEnd
  | otherwise = oldEnd + signum (newStart - oldEnd)

isRopeValid :: Point2D -> Point2D -> Bool
isRopeValid _start _end = squaredModulus (_start - _end) <= 2

toSteps :: Motion -> [Direction]
toSteps (Motion _direction _steps) = replicate _steps _direction

parseMotion :: String -> Motion
parseMotion line = Motion _direction _steps
  where
    _direction = parseDirection (head rawDirection)
    _steps = read rawSteps
    (rawDirection, rawSteps) = splitOn ' ' line

parseDirection :: Char -> Direction
parseDirection 'L' = L
parseDirection 'U' = U
parseDirection 'R' = R
parseDirection 'D' = D
parseDirection c = error ("Unknown direction: " ++ show c)

solution :: IO ()
solution = do
  motions <- parseInput <$> readFile "data/Day9/input"
  let allSteps = trackMotions motions
  let tailPositions = findTailVisitedPositions allSteps

  print (S.size tailPositions)
