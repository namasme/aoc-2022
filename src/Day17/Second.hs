{-# LANGUAGE TemplateHaskell #-}

module Day17.Second
  ( module Day17.Second
  , buildChamber
  , dropShape
  , parseInput
  ) where

import qualified Data.Map as M
import Lens.Micro.Platform
import Day17.First
  ( Chamber
  , Idx
  , buildChamber
  , dropShape
  , epoch
  , jetsIdx
  , parseInput
  , rows
  , shapeIdx
  )
import Utils.Common (iterateN)

data Cycle =
  Cycle
    { cycleLength :: Int
    , heightDiff :: Int
    }
  deriving (Eq, Show)
newtype ChamberHistory =
  ChamberHistory
    { _history :: M.Map ChamberKey [ChamberSnapshot]
    }
  deriving (Eq, Show)
type ChamberKey = (Idx, Idx)
type ChamberSnapshot = (Int, Int)

$(makeLenses ''ChamberHistory)

heightAfter :: Chamber -> Int -> Int
heightAfter chamber n = cycleRepetitions * diff + length (finalChamber ^. rows)
  where
    finalChamber = iterateN (n - lastEpoch) dropShape cyclingChamber
    lastEpoch = cycleStart + cycleRepetitions * size
    cycleRepetitions = (n - cycleStart) `div` size
    cycleStart = cyclingChamber ^. epoch
    (cyclingChamber, Cycle size diff) = findCycle chamber

findCycle :: Chamber -> (Chamber, Cycle)
findCycle chamber = findCycleAux chamber (ChamberHistory M.empty)

findCycleAux :: Chamber -> ChamberHistory -> (Chamber, Cycle)
findCycleAux chamber chamberHistory
  | length newSnapshots >= 3 =
    let ((epoch', height'):(epoch, height):_) = newSnapshots
     in (newChamber, Cycle
          { cycleLength = epoch' - epoch
          , heightDiff = height' - height
          })
  | otherwise = findCycleAux newChamber newHistory
  where
    newSnapshots = newHistory ^. history . at newKey . non []
    newHistory = chamberHistory & history . at newKey . non [] %~ (snapshotChamber newChamber :)
    newChamber = dropShape chamber
    newKey = keyChamber newChamber

keyChamber :: Chamber -> ChamberKey
keyChamber chamber = (chamber ^. shapeIdx . _1, chamber ^. jetsIdx . _1)

snapshotChamber :: Chamber -> ChamberSnapshot
snapshotChamber chamber = (chamber ^. epoch, length (chamber ^. rows))

solution :: IO ()
solution = do
  jetsSeed <- parseInput <$> readFile "data/Day17/input"

  let initialChamber = buildChamber jetsSeed

  print (heightAfter initialChamber 1000000000000)
