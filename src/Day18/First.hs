module Day18.First where

import Data.List.Split (splitOn)
import Utils.Spatial (Point3D(..))

countVisibleFaces :: [Point3D] -> Int
countVisibleFaces cubes = 6 * length cubes - 2 * countSharedFaces cubes

countSharedFaces :: [Point3D] -> Int
countSharedFaces [] = 0
countSharedFaces (cube:cubes) = sum (map (sharedFaces cube) cubes) + countSharedFaces cubes

sharedFaces :: Point3D -> Point3D -> Int
sharedFaces c c'
  | distance == 1 = 1
  | otherwise = 0
  where
    distance = l1 (c - c')

l1 :: Point3D -> Int
l1 p = x' + y' + z'
  where
    Point3D x' y' z' = abs p

parseInput :: String -> [Point3D]
parseInput = map parsePoint . lines

parsePoint :: String -> Point3D
parsePoint line = Point3D x y z
  where
    (x:y:z:_) = map read (splitOn "," line)

solution :: IO ()
solution = do
  cubes <- parseInput <$> readFile "data/Day18/input"

  print (countVisibleFaces cubes)
