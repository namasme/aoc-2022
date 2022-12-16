module Day12.First where

import Data.Char (ord)
import Data.Map ((!))
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Utils.Graphs (BFSManifest(..), runBFS)
import Utils.Spatial (Point2D(..), vonNeumannNeighbours)

type Elevation = Int
type Grid = M.Map Point2D Elevation

parseInput :: String -> (Point2D, Point2D, Grid)
parseInput input = (startPosition, endPosition, grid)
  where
    grid = M.fromList . zip coordinates . map toElevation . concat $ rows
    startPosition = fromJust (lookup 'S' rawGrid)
    endPosition = fromJust (lookup 'E' rawGrid)
    rawGrid = zip (concat rows) coordinates
    coordinates = map (toCoordinates width) [0 ..]
    width = length (head rows)
    rows = lines input

neighbours' :: Grid -> Point2D -> [Point2D]
neighbours' grid p = filter isValidNeighbour (vonNeumannNeighbours p)
  where
    isValidNeighbour p = maybe False (pElevation + 1 >=) $ M.lookup p grid
    pElevation = grid ! p

toCoordinates :: Int -> Int -> Point2D
toCoordinates width idx = Point2D x y
  where
    x = idx `mod` width
    y = idx `div` width

toElevation :: Char -> Elevation
toElevation 'S' = 0
toElevation 'E' = toElevation 'z'
toElevation c = ord c - ord 'a'

solution :: IO ()
solution = do
  (startPosition, endPosition, grid) <- parseInput <$> readFile "data/Day12/input"

  let isGoal' = (== endPosition)
  let manifest = BFSManifest startPosition (neighbours' grid) isGoal'

  print (runBFS manifest)
