{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Utils.Graphs where

import Control.Monad (guard)
import Data.Maybe (maybeToList)
import qualified Data.Map as M
import qualified Data.Set as S

data BFSManifest n where
  BFSManifest :: Ord n =>
    { origin :: n
    , neighbours :: n -> [n]
    , isGoal :: n -> Bool
    }
    -> BFSManifest n

type AdjacencyList n = [(n, n)]
type Distance = Int
newtype DistanceMatrix n =
  DistanceMatrix
    { distances :: M.Map (n, n) Distance
    } deriving (Eq, Show)

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

fromAdjacencyList :: Ord n => AdjacencyList n -> DistanceMatrix n
fromAdjacencyList edges = saturate (collectNodes edges) seed
  where
    seed = M.fromList [(buildKey u v, 1) | (u, v) <- edges]

collectNodes :: Ord n => AdjacencyList n -> [n]
collectNodes = S.toList . S.fromList . concatMap flatten
  where
    flatten (u, v) = [u, v]

buildKey :: Ord n => n -> n -> (n, n)
buildKey u v = (min u v, max u v)

getDistance :: Ord n => DistanceMatrix n -> n -> n -> Maybe Distance
getDistance = getDistance' . distances

getDistance' :: Ord n => M.Map (n, n) Distance -> n -> n -> Maybe Distance
getDistance' _distances u v = M.lookup (buildKey u v) _distances

saturate :: Ord n => [n] -> M.Map (n, n) Distance -> DistanceMatrix n
saturate nodes seed = DistanceMatrix $ foldl (bridgeBy nodes) seed nodes

bridgeBy :: Ord n => [n] -> M.Map (n, n) Distance -> n -> M.Map (n, n) Distance
bridgeBy nodes _distances w = M.unionWith min _distances . M.fromList $ do
  u <- nodes
  v <- nodes

  let isRedundant = u == v || u == w || v == w
  guard (not isRedundant)

  d1 <- maybeToList (getDistance' _distances u w)
  d2 <- maybeToList (getDistance' _distances w v)

  return (buildKey u v, d1 + d2)
