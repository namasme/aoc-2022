{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Utils.Graphs where

import qualified Data.Set as S

data BFSManifest n where
  BFSManifest :: Ord n =>
    { origin :: n
    , neighbours :: n -> [n]
    , isGoal :: n -> Bool
    }
    -> BFSManifest n

runBFS :: BFSManifest n -> Int
runBFS manifest = bfsAux manifest [(origin manifest, 0)] (S.singleton $ origin manifest)

-- You may be wondering, why the pattern matching if you declared record destructors?
-- See this answer for details https://stackoverflow.com/a/63188411
-- As to why I kept the record syntax, I feel like it makes the type more readable.
-- Open question: why is pattern matching not needed in the above definition for S.singleton?
bfsAux :: BFSManifest n -> [(n, Int)] -> S.Set n -> Int
bfsAux manifest@(BFSManifest _ _neighbours _isGoal) ((p, distance):pending) discovered
  | _isGoal p = distance
  | otherwise = bfsAux manifest newPending newDiscovered
  where
    newPending = pending ++ map (, distance + 1) nonDiscoveredNeighbours
    newDiscovered = S.union discovered $ S.fromList nonDiscoveredNeighbours
    nonDiscoveredNeighbours = filter (`S.notMember` discovered) (_neighbours p)
