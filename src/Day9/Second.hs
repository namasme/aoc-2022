module Day9.Second
  ( module Day9.Second
  , parseInput
  ) where

import qualified Data.Set as S

import Utils.Spatial (Direction(..), Point2D(..), move, squaredModulus)
import Day9.First (Motion, isRopeValid, parseInput, toSteps, updateEnd)

type KnottedRope = [Point2D]

initialRope = replicate 10 (Point2D 0 0)

findTailVisitedPositions :: [KnottedRope] -> S.Set Point2D
findTailVisitedPositions = S.fromList . map last

trackMotions :: [Motion] -> [KnottedRope]
trackMotions motions = scanl applyStep initialRope steps
  where
    steps = concatMap toSteps motions

applyStep :: KnottedRope -> Direction -> KnottedRope
applyStep [] _ = []
applyStep (k:knots) d = reverse . snd $ foldl updateKnot (initialHead, [initialHead]) knots
  where
    initialHead = move k d

updateKnot :: (Point2D, [Point2D]) -> Point2D -> (Point2D, [Point2D])
updateKnot (currentHead, reversedKnots) currentKnot =
  (newHead, newHead : reversedKnots)
  where
    newHead = updateEnd currentHead currentKnot

solution :: IO ()
solution = do
  motions <- parseInput <$> readFile "data/Day9/input"
  let allSteps = trackMotions motions
  let tailPositions = findTailVisitedPositions allSteps

  print (S.size tailPositions)
