module Day8.First where

import Data.Char (digitToInt)
import Data.List (transpose)
import qualified Data.Set as S

type Grid = [[Int]]

parseInput :: String -> Grid
parseInput = map (map digitToInt) . lines

gridVisibleIndices :: Grid -> S.Set (Int, Int)
gridVisibleIndices grid = S.union horizontalIndices verticalIndices
  where
    horizontalIndices = S.fromList . concatMap (uncurry attachRight) $ zip [0 ..] visiblePerRow
    verticalIndices = S.fromList . concatMap (uncurry attachLeft) $ zip [0 ..] visiblePerColumn
    visiblePerRow = map (S.toList . dimensionVisibleIndices) grid
    visiblePerColumn = map (S.toList . dimensionVisibleIndices) (transpose grid)
    attachLeft x xs = zip (repeat x) xs
    attachRight x xs = zip xs (repeat x)


dimensionVisibleIndices :: [Int] -> S.Set Int
dimensionVisibleIndices heights = S.union leftIndices rightIndices
  where
    leftIndices = orientedVisibleIndices heights
    rightIndices = S.map symmetricIndex . orientedVisibleIndices $ reverse heights
    symmetricIndex idx = length heights - idx - 1

orientedVisibleIndices :: [Int] -> S.Set Int
orientedVisibleIndices [] = error "The list of trees cannot be empty"
orientedVisibleIndices (h:heights) = S.insert 0 visibleIndices
  where
    visibleIndices = S.fromList . map fst . filter snd $ zip [1..] isVisible
    accumulatedMaximum = scanl max h heights
    isVisible = zipWith (>) heights accumulatedMaximum

solution :: IO ()
solution = do
  grid <- parseInput <$> readFile "data/Day8/input"

  print (S.size $ gridVisibleIndices grid)
