module Day18.Second
  ( module Day18.Second
  , parseInput
  ) where

import Data.List (foldl', partition)
import qualified Data.Set as S
import Day18.First (parseInput)
import Utils.Spatial (Point3D(..))

type DimensionBounds = (Int, Int)
data Bounds =
  Bounds
    { xBounds :: DimensionBounds
    , yBounds :: DimensionBounds
    , zBounds :: DimensionBounds
    } deriving (Eq, Show)

calculateExternalSurface :: [Point3D] -> Int
calculateExternalSurface cubes = countVisibleFaces bounds start (S.fromList cubes)
  where
    bounds = calculateBounds cubes
    start = Point3D (fst (xBounds bounds)) (fst (yBounds bounds)) (fst (zBounds bounds))

countVisibleFaces :: Bounds -> Point3D -> S.Set Point3D -> Int
countVisibleFaces bounds start droplet = floodFill bounds droplet [start] (S.singleton start) 0

floodFill :: Bounds -> S.Set Point3D -> [Point3D] -> S.Set Point3D -> Int -> Int
floodFill _ _ [] _ seenFaces = seenFaces
floodFill bounds droplet (p:pending) discovered seenFaces = floodFill bounds droplet newPending newDiscovered newSeenFaces
  where
    newPending = nonDiscoveredNeighbours ++ pending
    newDiscovered = S.union discovered $ S.fromList nonDiscoveredNeighbours
    nonDiscoveredNeighbours = filter (`S.notMember` discovered) nonDroplet
    newSeenFaces = seenFaces + length inDroplet
    (inDroplet, nonDroplet) = partition (`S.member` droplet) (neighbours bounds p)

neighbours :: Bounds -> Point3D -> [Point3D]
neighbours bounds p =
  filter
    (isValid bounds)
    [ p + Point3D 1 0 0
    , p + Point3D 0 1 0
    , p + Point3D 0 0 1
    , p + Point3D (-1) 0 0
    , p + Point3D 0 (-1) 0
    , p + Point3D 0 0 (-1)
    ]

calculateBounds :: [Point3D] -> Bounds
calculateBounds cubes = foldl' updateBounds initialBounds cubes
  where
    initialBounds =
      Bounds
        { xBounds = (x - 1, x + 1)
        , yBounds = (y - 1, y + 1)
        , zBounds = (z - 1, z + 1)
        }
    (Point3D x y z) = head cubes

updateBounds :: Bounds -> Point3D -> Bounds
updateBounds currentBounds (Point3D x y z) =
  Bounds
    { xBounds = updateDimensionBounds (xBounds currentBounds) x
    , yBounds = updateDimensionBounds (yBounds currentBounds) y
    , zBounds = updateDimensionBounds (zBounds currentBounds) z
    }

updateDimensionBounds :: DimensionBounds -> Int -> DimensionBounds
updateDimensionBounds (currentMin, currentMax) value
  | value <= currentMin = (value - 1, currentMax)
  | value >= currentMax = (currentMin, value + 1)
  | otherwise = (currentMin, currentMax)

isValid :: Bounds -> Point3D -> Bool
isValid bounds (Point3D x y z) =
  isValidDimensionValue (xBounds bounds) x &&
  isValidDimensionValue (yBounds bounds) y &&
  isValidDimensionValue (zBounds bounds) z

isValidDimensionValue :: DimensionBounds -> Int -> Bool
isValidDimensionValue (dimensionMin, dimensionMax) value = dimensionMin <= value && value <= dimensionMax

solution :: IO ()
solution = do
  cubes <- parseInput <$> readFile "data/Day18/input"

  let visibleFaces = calculateExternalSurface cubes

  print visibleFaces
