module Day22.First where

import Data.Char (isDigit)
import Data.List (span, unfoldr)
import Data.List.Extra (breakOn)
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import Data.Tuple (swap)
import Utils.Spatial
  ( Direction(..)
  , Point2D(..)
  , RotationalDirection(..)
  , rotate
  )

data PathStep = Move Int | Turn RotationalDirection deriving (Eq, Show)
type Path = [PathStep]
type Interval = (Int, Int)
type DimensionBoundaries = M.Map Int Interval
data Chart =
  Chart
    { rowBoundaries :: DimensionBoundaries
    , columnBoundaries :: DimensionBoundaries
    , walls :: S.Set Point2D
    } deriving (Eq, Show)
data Player =
  Player
    { position :: Point2D
    , facing :: Direction
    }
  deriving (Eq, Show)

calculatePassword :: Player -> Int
calculatePassword (Player (Point2D i j) direction) = 1000 * (i + 1) + 4 * (j + 1) + facingPassword direction
  where
    facingPassword R = 0
    facingPassword D = 1
    facingPassword L = 2
    facingPassword U = 3

applyPath :: Chart -> Player -> Path -> Player
applyPath chart = foldl (applyStep chart)

applyStep :: Chart -> Player -> PathStep -> Player
applyStep _ player (Turn direction) = player {facing = rotate (facing player) direction}
applyStep chart player (Move distance) = player {position = newPosition}
  where
    newPosition =
      head $
      [current | (current, next) <- zip segment (tail segment), isWall next] ++
      [last segment]
    segment = pathSegment chart player distance
    isWall p = S.member p (walls chart)

pathSegment :: Chart -> Player -> Int -> [Point2D]
pathSegment chart player@(Player (Point2D i j) _facing) distance
  | _facing == L || _facing == R = let
      dimensionSegment = take (distance + 1) (loop j boundaries)
      in [Point2D i j' | j' <- dimensionSegment]
  | otherwise = let
      dimensionSegment = take (distance + 1) (loop i boundaries)
      in [Point2D i' j | i' <- dimensionSegment]
  where
    boundaries = getBoundaries chart player

getBoundaries :: Chart -> Player -> (Int, Int)
getBoundaries chart (Player (Point2D i _) L) = swap (rowBoundaries chart ! i)
getBoundaries chart (Player (Point2D i _) R) = rowBoundaries chart ! i
getBoundaries chart (Player (Point2D _ j) U) = swap (columnBoundaries chart ! j)
getBoundaries chart (Player (Point2D _ j) D) = columnBoundaries chart ! j

loop :: Int -> (Int, Int) -> [Int]
loop start (lower, upper) = prefix ++ cycle fullLoop
  where
    prefix = enumFromThenTo start (start + delta) upper
    fullLoop = enumFromThenTo lower (lower + delta) upper
    delta = signum (upper - lower)

emptyChart :: Chart
emptyChart =
  Chart {rowBoundaries = M.empty, columnBoundaries = M.empty, walls = S.empty}

parseInput :: String -> (Point2D, Chart, Path)
parseInput rawInput = (findStartingPoint indexed, buildChart indexed, path)
  where
    (rawChart, rawPath) = breakOn "\n\n" rawInput
    indexed = index (lines rawChart)
    path = parsePath . drop 2 . init $ rawPath

index :: [String] -> [(Int, Int, Char)]
index rows = do
  (i, row) <- zip [0 ..] rows
  (j, tile) <- zip [0 ..] row

  return (i, j, tile)

findStartingPoint :: [(Int, Int, Char)] -> Point2D
findStartingPoint indexed = Point2D i j
  where
    (i, j, _) = head $ dropWhile (\(_, _, tile) -> tile == ' ') indexed

buildChart :: [(Int, Int, Char)] -> Chart
buildChart = foldl updateChart emptyChart

updateChart :: Chart -> (Int, Int, Char) -> Chart
updateChart (Chart _rowBoundaries _columnBoundaries _walls) (i, j, tile) =
  Chart
    { rowBoundaries = updateBoundaries i j tile _rowBoundaries
    , columnBoundaries = updateBoundaries j i tile _columnBoundaries
    , walls = updateWalls i j tile _walls
    }

updateBoundaries :: Int -> Int -> Char -> DimensionBoundaries -> DimensionBoundaries
updateBoundaries _ _ ' ' boundaries = boundaries
updateBoundaries which new _ boundaries = case M.lookup which boundaries of
  Nothing -> M.insert which (new, new) boundaries
  Just (lower, upper) -> M.insert which (min lower new, max upper new) boundaries

updateWalls :: Int -> Int -> Char -> S.Set Point2D -> S.Set Point2D
updateWalls i j '#' = S.insert (Point2D i j)
updateWalls _ _ _ = id

parsePath :: String -> Path
parsePath = unfoldr parsePathStep

parsePathStep :: String -> Maybe (PathStep, String)
parsePathStep "" = Nothing
parsePathStep rest = case span isDigit rest of
  ([], _) -> Just (Turn . parseTurn . head $ rest, tail rest)
  (number, rest') -> Just (Move (read number), rest')

parseTurn :: Char -> RotationalDirection
parseTurn 'L' = CCW
parseTurn 'R' = CW
parseTurn _ = error "unexpected turn"

buildInitialPlayer :: Point2D -> Player
buildInitialPlayer p = Player {position = p, facing = R}

solution :: IO ()
solution = do
  (start, chart, path) <- parseInput <$> readFile "data/Day22/input"
  let player = buildInitialPlayer start
  let finalPlayer = applyPath chart player path

  print (calculatePassword finalPlayer)
