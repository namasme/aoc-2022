module Day24.First where

import Data.Either (partitionEithers)
import qualified Data.Map as M
import Utils.Common (both, buildMultiMap)
import Utils.Graphs (AStarManifest(..), AStarResult(..), runAStar)
import Utils.Spatial (Point2D(..), manhattanDistance, vonNeumannNeighbours)

type Coordinate = Int
type Length = Int
type Time = Int
type AStarNode = (Time, Point2D)
data Facing
  = Backwards
  | Forwards
  deriving (Eq, Show)
data Blizzard = Blizzard
  { initialPosition :: Coordinate
  , facing :: Facing
  } deriving (Eq, Show)
data Valley = Valley
  { width :: Length
  , height :: Length
  , horizontalBlizzards :: M.Map Coordinate [Blizzard]
  , verticalBlizzards :: M.Map Coordinate [Blizzard]
  } deriving (Eq, Show)

findShortestPath :: Valley -> Time
findShortestPath valley = distanceTravelled (runAStar manifest)
  where
    manifest = AStarManifest origin (uncurry (neighbours valley)) isGoal heuristic
    origin = (0, valleyEntrance valley)
    isGoal = (== valleyExit valley) . snd
    heuristic = manhattanDistance (valleyExit valley) . snd

neighbours :: Valley -> Time -> Point2D -> [AStarNode]
neighbours valley time position =
  [ (time + 1, neighbour)
  | neighbour <- position : vonNeumannNeighbours position
  , isWithinBounds valley neighbour
  , isFree valley neighbour (time + 1)
  ]

isWithinBounds :: Valley -> Point2D -> Bool
isWithinBounds valley p@(Point2D i j)
  | p == valleyEntrance valley || p == valleyExit valley = True
  | otherwise = 1 <= i && i <= height valley && 1 <= j && j <= width valley

isFree :: Valley -> Point2D -> Time -> Bool
isFree valley (Point2D i j) time = not (rowHit || columnHit)
  where
    rowHit = dimensionHit (width valley) i j (horizontalBlizzards valley)
    columnHit = dimensionHit (height valley) j i (verticalBlizzards valley)
    dimensionHit length fixed target =
      any ((== target) . blizzardPositionAt length time) .
      M.findWithDefault [] fixed

valleyEntrance :: Valley -> Point2D
valleyEntrance _ = Point2D 0 1

valleyExit :: Valley -> Point2D
valleyExit valley = Point2D (height valley + 1) (width valley)

blizzardPositionAt :: Length -> Time -> Blizzard -> Coordinate
blizzardPositionAt length time blizzard =
  ((initialPosition blizzard - 1 + sign * time) `mod` length) + 1 -- trust me
  where
    sign =
      if facing blizzard == Forwards
        then 1
        else -1

parseInput :: String -> Valley
parseInput rawInput =
  Valley _width _height _horizontalBlizzards _verticalBlizzards
  where
    _width = length (head rows) - 2 -- do not count the walls on the sides
    _height = length rows - 2 -- do not count the walls on the sides
    (_verticalBlizzards, _horizontalBlizzards) =
      both buildMultiMap . partitionEithers $ blizzards
    blizzards =
      [ parseBlizzard i j tile
      | (i, row) <- zip [0 ..] rows
      , (j, tile) <- zip [0 ..] row
      , tile /= '.'
      , tile /= '#'
      ]
    rows = lines rawInput

parseBlizzard ::
     Coordinate
  -> Coordinate
  -> Char
  -> Either (Coordinate, Blizzard) (Coordinate, Blizzard)
parseBlizzard i j tile = case tile of
  -- Left and Right indicate which coordinate moves
  '^' -> Left (j, Blizzard i Backwards)
  '>' -> Right (i, Blizzard j Forwards)
  'v' -> Left (j, Blizzard i Forwards)
  '<' -> Right (i, Blizzard j Backwards)
  _ -> error "Unexpected blizzard tile, expected one of: <^>v"

solution :: IO ()
solution = do
  valley <- parseInput <$> readFile "data/Day24/input"

  print (findShortestPath valley)
