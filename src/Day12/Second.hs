module Day12.Second
  ( module Day12.Second
  , parseInput
  ) where

import Data.Map ((!))
import qualified Data.Map as M
import Utils.Graphs (BFSManifest(..), runBFS)
import Utils.Spatial (Point2D(..), vonNeumannNeighbours)

import Day12.First (Grid, parseInput)

neighbours' :: Grid -> Point2D -> [Point2D]
neighbours' grid p = filter isValidNeighbour (vonNeumannNeighbours p)
  where
    isValidNeighbour p = maybe False (pElevation - 1 <=) $ M.lookup p grid
    pElevation = grid ! p

isGoal' :: Grid -> Point2D -> Bool
isGoal' grid p = grid ! p == 0

solution :: IO ()
solution = do
  (_, endPosition, grid) <- parseInput <$> readFile "data/Day12/input"

  let manifest = BFSManifest endPosition (neighbours' grid) (isGoal' grid)

  print (runBFS manifest)
