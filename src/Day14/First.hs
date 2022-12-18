module Day14.First where

import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import qualified Data.Set as S
import Lens.Micro.Platform
import Utils.Common (iterateM)
import Utils.Spatial (Point2D(..), x, y)

type End = Point2D
data Cave =
  Cave
    { blocked :: S.Set Point2D
    , source :: Point2D
    , abyssDepth :: Int
    } deriving (Eq, Show)

parseInput :: String -> [[End]]
parseInput = map parseLine . lines

buildCave :: Point2D -> [[End]] -> Cave
buildCave _source endss = Cave _blocked _source _abyssDepth
  where
    _blocked = S.fromList points
    _abyssDepth = S.findMax . S.map (^. y) $ _blocked
    points = concatMap fillPath endss

tillOverflow :: Cave -> Int
tillOverflow = length . takeWhile isJust . tail . iterateM spawnSand . Just

spawnSand :: Cave -> Maybe Cave
spawnSand cave@(Cave _blocked _source _abyssDepth) = do
  newTile <- evolveSand cave _source
  let newBlocked = S.insert newTile _blocked

  return (Cave newBlocked _source _abyssDepth)

evolveSand :: Cave -> Point2D -> Maybe Point2D
evolveSand cave@(Cave _blocked _ _abyssDepth) p
  | p ^. y > _abyssDepth = Nothing
  | otherwise =
    case next of
      (Just p') -> evolveSand cave p'
      Nothing -> Just p
  where
    next =
      safeHead . filter (`S.notMember` _blocked) . map (p +) $
      [Point2D 0 1, Point2D (-1) 1, Point2D 1 1]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

fillSegment :: End -> End -> [Point2D]
fillSegment p p'
  | p == p' = [p']
  | otherwise = p:fillSegment (p + delta) p'
  where
    delta = signum (p' - p)

fillPath :: [End] -> [Point2D]
fillPath ends =
  head ends : concatMap (tail . uncurry fillSegment) (zip ends (tail ends))

parseLine :: String -> [End]
parseLine = map parsePoint . splitOn " -> "

parsePoint :: String -> Point2D
parsePoint = (\[column, row] -> Point2D column row) . map read . splitOn ","

solution :: IO ()
solution = do
  endss <- parseInput <$> readFile "data/Day14/input"
  let _source = Point2D 500 0
  let cave = buildCave _source endss

  print (tillOverflow cave)
