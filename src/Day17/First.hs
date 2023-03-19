{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
module Day17.First where

import Data.Bits ((.&.), (.|.), shift, testBit)
import Data.Word (Word8(..))
import Lens.Micro.Platform
import Utils.Common (iterateN)

data JetDirection
  = L
  | R
  deriving (Eq, Show)
data Shape
  = HLine
  | Cross
  | LShape
  | VLine
  | Square
  deriving (Eq, Show)
type Row = Word8
type Idx = Int

toRows :: Shape -> [Row]
toRows HLine = [30]
toRows Cross = [8, 28, 8]
toRows LShape = [4, 4, 28]
toRows VLine = [16, 16, 16, 16]
toRows Square = [24, 24]

data Chamber =
  Chamber
    { _rows :: [Row]
    , _shapes :: [Shape]
    , _shapeIdx :: (Idx, Int)
    , _jets :: [JetDirection]
    , _jetsIdx :: (Idx, Int)
    , _epoch :: Int
    }
  deriving (Eq, Show)

$(makeLenses ''Chamber)

heightAfter :: Chamber -> Int -> Int
heightAfter initialChamber n = length finalRows
  where
    finalRows = iterateN n dropShape initialChamber ^. rows

dropShape :: Chamber -> Chamber
dropShape chamber = advanceShape (simulateFall chamber) & epoch %~ succ

simulateFall :: Chamber -> Chamber
simulateFall chamber = simulateFallAux initialShapeRows [] paddedChamber & rows %~ removeEmptyRows
  where
    paddedChamber = chamber & rows %~ (replicate padding 0 ++)
    padding = 3 + length initialShapeRows
    initialShapeRows = toRows (currentShape chamber)
    removeEmptyRows = dropWhile (== 0)

advanceJet :: Chamber -> Chamber
advanceJet chamber = chamber & jets %~ tail & jetsIdx %~ next

currentJet :: Chamber -> JetDirection
currentJet chamber = head (chamber ^. jets)

advanceShape :: Chamber -> Chamber
advanceShape chamber = chamber & shapes %~ tail & shapeIdx %~ next

currentShape :: Chamber -> Shape
currentShape chamber = head (chamber ^. shapes)

simulateFallAux :: [Row] -> [Row] -> Chamber -> Chamber
simulateFallAux shapeRows discardedRows chamber = case remainingRows of
  Nothing -> simulateFallAux pushedShapeRows (topRow:discardedRows) (chamber' & rows .~ tailRows)
  Just finalRows -> chamber' & rows .~ reverse discardedRows ++ finalRows
  where
    (pushedShapeRows, remainingRows) = step (currentJet chamber) shapeRows (chamber ^. rows)
    chamber' = advanceJet chamber
    topRow = head (chamber ^. rows)
    tailRows = tail (chamber ^. rows)

step :: JetDirection -> [Row] -> [Row] -> ([Row], Maybe [Row])
step jet shapeRows remainingRows = (pushedShapeRows, attemptFall pushedShapeRows remainingRows)
  where
    pushedShapeRows = attemptPush jet shapeRows remainingRows

attemptPush :: JetDirection -> [Row] -> [Row] -> [Row]
attemptPush jet shapeRows remainingRows
  | collides jet shapeRows remainingRows = shapeRows
  | otherwise = map (toShift jet) shapeRows

attemptFall :: [Row] -> [Row] -> Maybe [Row]
attemptFall shapeRows remainingRows
  | length remainingRows > length shapeRows && canFall = Nothing
  | otherwise = Just (zipWithLongest (.|.) shapeRows remainingRows)
  where
    canFall = all (== 0) $ zipWith (.&.) shapeRows (tail remainingRows)

collides :: JetDirection -> [Row] -> [Row] -> Bool
collides jet shapeRows remainingRows
  | jet == R && any (rowCollision 0b0000001) shapeRows = True
  | jet == L && any (rowCollision 0b1000000) shapeRows = True
  | otherwise = or . zipWith rowCollision remainingRows . map (toShift jet) $ shapeRows
  where
    rowCollision row row' = row .&. row' /= 0

zipWithLongest :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithLongest _ xs [] = xs
zipWithLongest _ [] ys = ys
zipWithLongest f (x:xs) (y:ys) = f x y : zipWithLongest f xs ys

toShift :: JetDirection -> Row -> Row
toShift L = flip shift 1
toShift R = flip shift (-1)

next :: (Idx, Int) -> (Idx, Int)
next (i, m) = ((i + 1) `mod` m, m)

buildChamber :: [JetDirection] -> Chamber
buildChamber jetsSeed =
  Chamber
    { _rows = []
    , _shapes = cycle orderedShapes
    , _shapeIdx = (0, length orderedShapes)
    , _jets = cycle jetsSeed
    , _jetsIdx = (0, length jetsSeed)
    , _epoch = 0
    }
  where
    orderedShapes = [HLine, Cross, LShape, VLine, Square]

parseInput :: String -> [JetDirection]
parseInput = map parseJet . takeWhile (/= '\n') -- trim trailing newline

parseJet :: Char -> JetDirection
parseJet '<' = L
parseJet '>' = R
parseJet c = error ("Unexpected character: " ++ show c)

solution :: IO ()
solution = do
  jetsSeed <- parseInput <$> readFile "data/Day17/input"

  let initialChamber = buildChamber jetsSeed

  print (heightAfter initialChamber 2022)
