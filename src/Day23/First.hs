module Day23.First where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (foldl', foldl1')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Lens.Micro.Platform
import Utils.Common (iterateN)
import Utils.Spatial
  ( Direction(..)
  , Point2D(..)
  , RotationalDirection(..)
  , mooreNeighbours
  , move
  , rotate
  )

data Elves = Elves
  { positions :: S.Set Point2D
  , directionSuggestions :: [Direction]
  } deriving (Eq, Show)

calculateEmptyTiles :: Elves -> Int
calculateEmptyTiles elves = width * height - S.size (positions elves)
  where
    width = j' - j + 1
    height = i' - i + 1
    (Point2D i j, Point2D i' j') = boundingRectangle . S.toList $ positions elves

applyStep :: Elves -> Elves
applyStep elves =
  Elves
    { positions = newPositions
    , directionSuggestions = tail (directionSuggestions elves)
    }
  where
    newPositions =
      S.fromList $
      concat
        [ if length applicants > 1
          then applicants
          else [target]
        | (target, applicants) <- M.toList summary
        ]
    summary = foldl' (updateSummary elves) M.empty (S.toList $ positions elves)

updateSummary :: Elves -> M.Map Point2D [Point2D] -> Point2D -> M.Map Point2D [Point2D]
updateSummary elves partialSummary current =
  partialSummary & at (proposeDestination elves current) . non [] %~ (current :)

proposeDestination :: Elves -> Point2D -> Point2D
proposeDestination elves current
  | isIsolated = current
  | otherwise = fromMaybe current candidate
  where
    isIsolated = all (`S.notMember` positions elves) (mooreNeighbours current)
    candidate =
      foldl1'
        (<|>)
        [ attemptMove elves direction current
        | direction <- take 4 (directionSuggestions elves)
        ]

attemptMove :: Elves -> Direction -> Point2D -> Maybe Point2D
attemptMove elves position direction
  | any (`S.member` positions elves) _neighbours = Nothing
  | otherwise = Just (head _neighbours)
  where
    _neighbours = neighbours position direction

neighbours :: Direction -> Point2D -> [Point2D]
neighbours direction position = [center, left, right]
  where
    left = move center (rotate direction CW)
    right = move center (rotate direction CCW)
    center = move position direction

boundingRectangle :: [Point2D] -> (Point2D, Point2D)
boundingRectangle [] = error "Invalid input: list cannot be empty"
boundingRectangle (Point2D x y:ps) = (Point2D i j, Point2D i' j')
  where
    (i, j, i', j') = foldl' updateBounds (x, y, x, y) ps
    updateBounds (i, j, i', j') (Point2D i'' j'') =
      (min i i'', min j j'', max i' i'', max j' j'')

parseInput :: String -> Elves
parseInput rawInput =
  -- The coordinate system for the maze is not the one move expects, so the
  -- directions have to be adjusted accordingly.
  -- | Intended | Actual | Delta   |
  -- |----------|--------|---------|
  -- | L        | D      | (0, -1) |
  -- | U        | L      | (-1, 0) |
  -- | R        | U      | (0, 1)  |
  -- | D        | R      | (1, 0)  |
  Elves {positions = _positions, directionSuggestions = cycle [L, R, D, U]}
  where
    _positions =
      S.fromList
        [ Point2D rowIdx columnIdx
        | (rowIdx, row) <- zip [0 ..] (lines rawInput)
        , (columnIdx, tile) <- zip [0 ..] row
        , tile /= '.'
        ]

solution :: IO ()
solution = do
  elves <- parseInput <$> readFile "data/Day23/input"
  let finalElves = iterateN 10 applyStep elves

  print (calculateEmptyTiles finalElves)
