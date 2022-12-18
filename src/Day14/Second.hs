module Day14.Second
  ( module Day14.Second
  , parseInput
  , Cave(..)
  ) where

import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Lens.Micro.Platform
import Utils.Spatial (Point2D(..), x, y)

import Day14.First (Cave(..), End, buildCave, parseInput, safeHead)

tillSource :: Cave -> Int
tillSource cave@(Cave _blocked _source _)
  | S.member _source _blocked = 0
  | otherwise = 1 + tillSource (spawnSand cave)

spawnSand :: Cave -> Cave
spawnSand cave@(Cave _blocked _source floorDepth) =
  Cave newBlocked _source floorDepth
  where
    newBlocked = S.insert landingPosition _blocked
    landingPosition = evolveSand cave _source

buildCave' :: Point2D -> [[End]] -> Cave
buildCave' _source endss = Cave _blocked _source (_abyssDepth + 2)
  where
    (Cave _blocked _ _abyssDepth) = buildCave _source endss

evolveSand :: Cave -> Point2D -> Point2D
evolveSand cave@(Cave _blocked _ floorDepth) p = fromMaybe p next
  where
    next =
      evolveSand cave <$>
      (safeHead . filter isFree . map (p +) $
       [Point2D 0 1, Point2D (-1) 1, Point2D 1 1])
    isFree q = S.notMember q _blocked && q ^. y < floorDepth

solution :: IO ()
solution = do
  endss <- parseInput <$> readFile "data/Day14/input"
  let _source = Point2D 500 0
  let cave = buildCave' _source endss

  print (tillSource cave)
