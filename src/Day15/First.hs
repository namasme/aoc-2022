module Day15.First
  ( module Day15.First
  , Sensor(..)
  , parseInput
  ) where

import Control.Monad (guard)
import Data.Function (on)
import Data.List (nub, sortBy)
import Data.Maybe (mapMaybe)
import Day15.First.Internal (Beacon, Sensor(..), parseInput)
import Utils.Spatial (Point2D(..), getX, getY)

type Interval = (Int, Int)

impossibleBeaconsCount :: Int -> [Sensor] -> [Beacon] -> Int
impossibleBeaconsCount row sensors beacons = combinedSize section - _beaconsInSectionCount
  where
    section = computeSection row sensors
    _beaconsInSectionCount = beaconsInSectionCount row beacons section

computeSection :: Int -> [Sensor] -> [Interval]
computeSection row = intervalUnion . mapMaybe (rowSlice row)

combinedSize :: [Interval] -> Int
combinedSize = sum . map (succ . abs . uncurry (-))

beaconsInSectionCount :: Int -> [Beacon] -> [Interval] -> Int
beaconsInSectionCount row beacons intervals = length $ do
  Point2D x y <- nub beacons

  guard (y == row)
  guard (any (liesIn x) intervals)

  return ()

liesIn :: Int -> Interval -> Bool
liesIn x (xMin, xMax) = xMin <= x && x <= xMax

intervalUnion :: [Interval] -> [Interval]
intervalUnion = foldr addInterval [] . sortBy (compare `on` snd)

addInterval :: Interval -> [Interval] -> [Interval]
addInterval interval [] = [interval]
addInterval (xMin', xMax') ((xMin, xMax):rest)
  | xMax' >= xMin = (min xMin xMin', xMax):rest
  | otherwise = (xMin', xMax'):(xMin, xMax):rest

rowSlice :: Int -> Sensor -> Maybe Interval
rowSlice y sensor = do
  let (Point2D i j) = center sensor
  let yDistance = abs (j - y)

  guard (yDistance <= radius sensor)

  let xDelta = radius sensor - yDistance

  return (i - xDelta, i + xDelta)

solution :: IO ()
solution = do
  (sensors, beacons) <- parseInput <$> readFile "data/Day15/input"
  let row = 2000000

  print (impossibleBeaconsCount row sensors beacons)
