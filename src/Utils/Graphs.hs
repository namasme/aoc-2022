{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Utils.Graphs where

import Control.Monad (guard)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S

data BFSManifest n where
  BFSManifest
    :: Ord n => n -> NeighboursSupplier n -> IsGoalPredicate n -> BFSManifest n


type NeighboursSupplier n = n -> [n]
type IsGoalPredicate n = n -> Bool
type Heuristic n = n -> Int
data AStarManifest n where
  AStarManifest
    :: Ord n => n
    -> NeighboursSupplier n
    -> IsGoalPredicate n
    -> Heuristic n
    -> AStarManifest n

data AStarResult n = AStarResult
  { distanceTravelled :: Int
  , path :: [n]
  } deriving (Eq, Show)

type AdjacencyList n = [(n, n)]
type Distance = Int
newtype DistanceMatrix n =
  DistanceMatrix
    { distances :: M.Map (n, n) Distance
    } deriving (Eq, Show)

runBFS :: BFSManifest n -> Int
runBFS manifest@(BFSManifest origin _ _) = bfsAux manifest [(origin, 0)] (S.singleton origin)

runAStar :: AStarManifest n -> AStarResult n
runAStar manifest@(AStarManifest origin _ _ _) = aStarAux manifest pQueue cameFrom partialDistances
  where
    pQueue = P.fromList [(0, (0, origin))]
    cameFrom = M.empty
    partialDistances = M.singleton origin 0

bfsAux :: BFSManifest n -> [(n, Int)] -> S.Set n -> Int
bfsAux manifest@(BFSManifest _ neighbours isGoal) ((p, distance):pending) discovered
  | isGoal p = distance
  | otherwise = bfsAux manifest newPending newDiscovered
  where
    newPending = pending ++ map (, distance + 1) nonDiscoveredNeighbours
    newDiscovered = S.union discovered $ S.fromList nonDiscoveredNeighbours
    nonDiscoveredNeighbours = filter (`S.notMember` discovered) (neighbours p)

-- TODO: currentDistance is probably unnecessary and can be queried from partialDistances
aStarAux :: AStarManifest n -> P.MinPQueue Int (Int, n) -> M.Map n n -> M.Map n Int -> AStarResult n
aStarAux manifest@(AStarManifest _ neighbours isGoal heuristic) open cameFrom partialDistances
  | isGoal current =
    AStarResult
      {distanceTravelled = currentDistance, path = tracePath current cameFrom}
  | otherwise = aStarAux manifest newOpen newCameFrom newPartialDistances
  where
    ((_, (currentDistance, current)), rest) = P.deleteFindMin open
    newOpen =
      P.union
        rest
        (P.fromList
           [ ( currentDistance + 1 + heuristic neighbour
             , (currentDistance + 1, neighbour))
           | neighbour <- pending
           ])
    (newCameFrom, newPartialDistances) =
      foldl updateState (cameFrom, partialDistances) pending
    updateState (cameFrom', partialDistances') neighbour =
      ( M.insert neighbour current cameFrom'
      , M.insert neighbour (currentDistance + 1) partialDistances')
    pending =
      exploreNeighboursAStar
        (currentDistance, current)
        neighbours
        partialDistances

-- TODO: path is reversed, starting from the end. It should start from the origin.
tracePath :: Ord n => n -> M.Map n n -> [n]
tracePath end cameFrom = case prev of
  Nothing -> [end]
  Just prev' -> end:tracePath prev' cameFrom
  where
    prev = M.lookup end cameFrom

exploreNeighboursAStar :: Ord n => (Int, n) -> (n -> [n]) -> M.Map n Int -> [n]
exploreNeighboursAStar (currentDistance, current) neighbours partialDistances =
  [ neighbour
  | neighbour <- neighbours current
  -- default to True if neighbour does not have a partial distance yet
  , maybe True (currentDistance + 1 <) (M.lookup neighbour partialDistances)
  ]

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
