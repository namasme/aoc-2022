module Day22.CubeParser where

import Control.Monad (guard)
import Data.Maybe (maybeToList)
import Data.List.Extra (breakOn, transpose)
import Data.List.Split (chunksOf, splitPlaces)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple (swap)
import Utils.Spatial
  ( Direction(..)
  , Point2D(..)
  , measureOrientation
  , opposite
  , rotate
  )
import Day22.First (Path(..), findStartingPoint, index, parsePath, Chart (rowBoundaries))

type CubeFaceID = Int
type FaceBoundaries = (Point2D, Point2D)
type Boundaries = M.Map CubeFaceID FaceBoundaries
type Borders = M.Map (CubeFaceID, Direction) (CubeFaceID, Direction)
data Atlas = Atlas
  { boundaries :: Boundaries
  , walls :: S.Set Point2D
  , borders :: Borders
  , cubeFaceSize :: Int
  } deriving (Eq, Show)

parseAtlas :: [String] -> Atlas
parseAtlas rows =
  Atlas
    { borders = borders
    , boundaries = boundaries
    , walls = walls
    , cubeFaceSize = cubeFaceSize
    }
  where
    borders = assembleFaces faces
    boundaries = computeBoundaries faces cubeFaceSize
    walls = collectWalls rows
    (faces, cubeFaceSize) = findFaces rows

computeBoundaries :: [[CubeFaceID]] -> Int -> Boundaries
computeBoundaries faces cubeFaceSize = M.fromList $ do
  (blockRowIdx, blockRow) <- zip [0..] faces
  (blockColumnIdx, faceId) <- zip [0..] blockRow

  guard (faceId /= faceIDSentinel)

  let topLeft = fromIntegral cubeFaceSize * Point2D blockRowIdx blockColumnIdx
  let bottomRight = topLeft + Point2D (cubeFaceSize - 1) (cubeFaceSize - 1)
  return (faceId, (topLeft, bottomRight))

findFaces :: [String] -> ([[CubeFaceID]], Int)
findFaces rows = (faces, cubeFaceSize)
  where
    faces = indexFaces representatives
    representatives = -- top left corner from each block
      [ [head firstBlockRow | firstBlockRow <- head blockRow]
      | blockRow <- chunked
      ]
    chunked = chunksOf cubeFaceSize chunkedRows
    chunkedRows = map (chunksOf cubeFaceSize) rows
    cubeFaceSize = foldl1 gcd (map length rows)

assembleFaces :: [[CubeFaceID]] -> Borders
assembleFaces faces = saturateBorders initialBorders
  where
    initialBorders = initialFacesAssembly faces

initialFacesAssembly :: [[CubeFaceID]] -> Borders
initialFacesAssembly faces = M.union byRows byColumns
  where
    byRows = initialBordersDimension faces R L
    byColumns = initialBordersDimension (transpose $ pad faces) D U

-- This is probably not necessary for cubes because you cannot get two
-- non-contiguous faces in the same column, but just in case
pad :: [[CubeFaceID]] -> [[CubeFaceID]]
pad rows = map padRow rows
  where
    maxLength = maximum (map length rows)
    padRow row = row ++ replicate (maxLength - length row) faceIDSentinel

initialBordersDimension :: [[CubeFaceID]] -> Direction -> Direction -> Borders
initialBordersDimension faces prev next =
  M.fromList
    [ ((face, prev), (face', next))
    | row <- faces
    , (face, face') <- zip row (tail row)
    , face /= faceIDSentinel && face' /= faceIDSentinel
    ]

saturateBorders :: Borders -> Borders
saturateBorders borders
  | not (M.null newBorders) = saturateBorders (M.union borders newBorders)
  | otherwise = symmetricEdges borders
  where
    symmetricEdges = M.fromList . concatMap duplicate . M.toList
    duplicate edge = [edge, swap edge]
    entries = M.toList borders
    newBorders = M.fromList $  do
      ((a, da), (b, db)) <- entries
      ((c, dc), (d, dd)) <- entries

      edge <- maybeToList $ connectEdge (a, da) (b, db) (c, dc) (d, dd)
      guard (M.notMember (fst edge) borders)

      return edge

connectEdge ::
     (CubeFaceID, Direction)
  -> (CubeFaceID, Direction)
  -> (CubeFaceID, Direction)
  -> (CubeFaceID, Direction)
  -> Maybe ((CubeFaceID, Direction), (CubeFaceID, Direction))
connectEdge (a, da) (b, db) (c, dc) (d, dd)
  | a == c = connectEdgeAux (b, db) (a, da, dc) (d, dd)
  | a == d = connectEdgeAux (b, db) (a, da, dd) (c, dc)
  | b == c = connectEdgeAux (a, da) (b, db, dc) (d, dd)
  | b == d = connectEdgeAux (a, da) (b, db, dd) (c, dc)
  | otherwise = Nothing

connectEdgeAux ::
     (CubeFaceID, Direction)
  -> (CubeFaceID, Direction, Direction)
  -> (CubeFaceID, Direction)
  -> Maybe ((CubeFaceID, Direction), (CubeFaceID, Direction))
connectEdgeAux (a, da) (b, db, db') (c, dc) = do
  guard (db /= db')
  orientation <- measureOrientation db db'
  let da' = rotate da (opposite orientation)
  let dc' = rotate dc orientation

  return ((a, da'), (c, dc'))

indexFaces :: [[Char]] -> [[CubeFaceID]]
indexFaces blocks = splitPlaces (map length blocks) $ do
  (index, tile) <- zip [1..] (concat blocks)
  return (if tile == ' ' then faceIDSentinel else index)

faceIDSentinel :: CubeFaceID
faceIDSentinel = 0

collectWalls :: [String] -> S.Set Point2D
collectWalls rows =
  S.fromList $ do
    (rowIdx, row) <- zip [0 ..] rows
    (columnIdx, tile) <- zip [0 ..] row
    guard (tile == '#')

    return (Point2D rowIdx columnIdx)
