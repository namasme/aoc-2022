module Day8.Second
  ( module Day8.Second
  , parseInput
  ) where

import Debug.Trace
import Data.Function (on)
import Data.List (sortBy, transpose)

import Day8.First (Grid, parseInput)

type ViewingDistance = Int
type Position = Int

maxScenicScore :: Grid -> Int
maxScenicScore = maximum . concat . scenicScore

scenicScore :: Grid -> Grid
scenicScore grid = hadamardMap (*) horizontalScenicScores verticalScenicScores
  where
    horizontalScenicScores = hadamardMap (*) leftViewingDistances rightViewingDistances
    verticalScenicScores = hadamardMap (*) topViewingDistances bottomViewingDistances
    leftViewingDistances = map rollingViewingDistance grid
    rightViewingDistances = map reverseRollingViewingDistance grid
    topViewingDistances = transpose . map rollingViewingDistance . transpose $ grid
    bottomViewingDistances = transpose . map reverseRollingViewingDistance . transpose $ grid

hadamardMap :: (Int -> Int -> Int) -> Grid -> Grid -> Grid
hadamardMap f = zipWith (zipWith f)

rollingViewingDistance :: [Int] -> [Int]
rollingViewingDistance xs = rollingViewingDistanceAux (zip [0 ..] xs) [] []

reverseRollingViewingDistance :: [Int] -> [Int]
reverseRollingViewingDistance = reverse . rollingViewingDistance . reverse

rollingViewingDistanceAux :: [(Position, Int)] -> [(Position, Int)] -> [(Position, ViewingDistance)] -> [ViewingDistance]
rollingViewingDistanceAux [] [] finalVisibilities = map snd . sortBy (compare `on` fst) $ finalVisibilities
rollingViewingDistanceAux [] pending blocked =
  rollingViewingDistanceAux [] [] finalVisibilities
  where
    finalVisibilities = blocked ++ pendingVisibilities
    pendingVisibilities = [(position, visibilityFromIndex position) | (position, _) <- pending]
    visibilityFromIndex i = totalSize - i - 1
    totalSize = length pending + length blocked
    size = length pending
rollingViewingDistanceAux (u:unvisited) pending blocked = rollingViewingDistanceAux unvisited newPending newBlocked
 where
   newPending = u:pendingNotBlocked
   newBlocked = [(position, fst u - position) | (position, _) <- pendingBlocked] ++ blocked
   (pendingBlocked, pendingNotBlocked) = span (`isBlockedBy` u) pending
   isBlockedBy = (<=) `on` snd

solution :: IO ()
solution = do
  grid <- parseInput <$> readFile "data/Day8/input"

  print (maxScenicScore grid)
