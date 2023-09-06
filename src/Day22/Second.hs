module Day22.Second where

import Data.List.Extra (breakOn)
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import Utils.Common (both, iterateN, takeUntil)
import Utils.Spatial
  ( Direction(..)
  , Point2D(..)
  , RotationalDirection(..)
  , move
  , revert
  , rotate
  )
import Day22.First
  ( Chart(rowBoundaries)
  , Path(..)
  , PathStep(..)
  , Player(..)
  , buildInitialPlayer
  , calculatePassword
  , findStartingPoint
  , index
  , parsePath
  )
import Day22.CubeParser (Atlas(..), CubeFaceID, findFaces, parseAtlas)

applyPath :: Atlas -> Player -> Path -> Player
applyPath atlas = foldl (applyStep atlas)

applyStep :: Atlas -> Player -> PathStep -> Player
applyStep _ player (Turn direction) = player {facing = rotate (facing player) direction}
applyStep atlas player (Move distance) = newPlayer
  where
    newPlayer =
      head $
      [current | (current, next) <- zip segment (tail segment), isWall next] ++
      [last segment]
    segment = pathSegment atlas player distance
    isWall player' = S.member (position player') (walls atlas)

pathSegment :: Atlas -> Player -> Int -> [Player]
pathSegment atlas player distance = take (distance + 1) . loopFrom atlas $ player

loopFrom :: Atlas -> Player -> [Player]
loopFrom atlas player@(Player (Point2D i j) _facing) = prefix ++ loop
  where
    prefix = completeFace atlas player
    loop = cycle . concat . take 4 . tail . iterate newFace $ prefix
    newFace lastFace =
      let playerOnBorder = last lastFace
          playerAfterBorder = crossBorder atlas playerOnBorder
       in completeFace atlas playerAfterBorder

-- This function requires the player to be on the verge of crossing a face
-- border. Result is undefined (incorrect) otherwise.
crossBorder :: Atlas -> Player -> Player
crossBorder atlas player = Player newPosition newFacing
  where
    newPosition = newTopLeft + newLocalPosition
    newTopLeft = fst (boundaries atlas ! newFace)
    newLocalPosition = -- this is the local position in the new face, after having crossed
      modCoordinates (cubeFaceSize atlas) .
        move' newFacing $ mappedLocalPosition
    mappedLocalPosition = mapPosition localPosition (facing player) newFacing (cubeFaceSize atlas)
    newFacing = revert borderDirection
    localPosition = position player - topLeft
    topLeft = fst (boundaries atlas ! currentFace)
    (newFace, borderDirection) = borders atlas ! (currentFace, facing player)
    currentFace = findFace atlas (position player)

mapPosition :: Point2D -> Direction -> Direction -> Int -> Point2D
mapPosition currentPosition currentFacing newFacing cubeFaceSize = newPosition
  where
    newPosition = iterateN requiredRotations applyCWRotation currentPosition
    applyCWRotation (Point2D i j) = modCoordinates cubeFaceSize (Point2D j (-i - 1))
    requiredRotations =
      length . takeWhile (/= newFacing) . iterate (`rotate` CW) $
      currentFacing

modCoordinates :: Int -> Point2D -> Point2D
modCoordinates modulo (Point2D i j) = Point2D (i `mod` modulo) (j `mod` modulo)

completeFace :: Atlas -> Player -> [Player]
completeFace atlas player@(Player (Point2D i j) _facing)
  | isFaceBorder atlas player = [player]
  | otherwise = takeUntil (isFaceBorder atlas) ray
  where
    ray =
      [ player {position = newPosition}
      | newPosition <- iterate (move' _facing) (Point2D i j)
      ]

-- Adapted version of move to the coordinate system used in this solution.
-- It also reverses the order of the parameters to simplify application.
move' :: Direction -> Point2D -> Point2D
move' direction p = move p newDirection
  where
    -- This rotation is necessary because of the x-y swap. E.g., move p U
    -- will increase p's second coordinate by one, when in this case we want
    -- to decrease the first coordinate (equivalent to move p (rotate U CCW) == move p L).
    newDirection = rotate direction CCW

isFaceBorder :: Atlas -> Player  -> Bool
isFaceBorder (Atlas _ _ _ _cubeFaceSize) (Player (Point2D i j) _facing) =
  case _facing of
    L -> columnMod == 0
    U -> rowMod == 0
    R -> columnMod == (_cubeFaceSize - 1)
    D -> rowMod == (_cubeFaceSize - 1)
  where
    rowMod = i `mod` _cubeFaceSize
    columnMod = j `mod` _cubeFaceSize

findFace :: Atlas -> Point2D -> CubeFaceID
findFace atlas (Point2D i j) =
  head
    [ faceID
    | (faceID, (Point2D top left, Point2D bottom right)) <- boundariesEntries
    , top <= i
    , i <= bottom
    , left <= j
    , j <= right
    ]
  where
    boundariesEntries = M.toList (boundaries atlas)

parseInput :: String -> (Point2D, Atlas, Path)
parseInput rawInput = (startingPoint, atlas, path)
  where
    startingPoint = findStartingPoint indexed
    atlas = parseAtlas (lines rawChart)
    path = parsePath . drop 2 . init $ rawPath
    (rawChart, rawPath) = breakOn "\n\n" rawInput
    indexed = index (lines rawChart)

solution :: IO ()
solution = do
  (start, atlas, path) <- parseInput <$> readFile "data/Day22/input"
  let player = buildInitialPlayer start
  let finalPlayer = applyPath atlas player path

  print (calculatePassword finalPlayer)
