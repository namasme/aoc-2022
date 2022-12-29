module Day16.First
  ( module Day16.First
  , Valve(..)
  , ValveName
  , parseInput
  ) where

{-
This solution was *heavily inspired* by @juanplopes'
https://github.com/juanplopes/advent-of-code-2022/blob/main/day16.py
-}

import Control.Monad.Writer
  ( MonadWriter(tell)
  , Writer
  , execWriter
  , guard
  , runWriter
  )
import Data.Bits ((.&.), (.|.), bit)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Utils.Common (iterateM)
import Utils.Graphs
  ( AdjacencyList
  , DistanceMatrix(..)
  , fromAdjacencyList
  , getDistance
  )
import Day16.First.Internal (Valve(..), ValveName, parseInput)

type Distance = Int
type Time = Int
type BitMask = Integer
type ValveEncoding = [(ValveName, BitMask)]
data Path =
  Path
    { currentValve :: ValveName
    , currentPressure :: Int
    , remainingTime :: Time
    , visited :: BitMask
    }
  deriving (Eq, Show)

data Sewer =
  Sewer
    { nameToValve :: M.Map ValveName Valve
    , distanceMatrix :: DistanceMatrix ValveName
    , valveEncoding :: ValveEncoding
    }

newtype PathOptimizer =
  PathOptimizer
    { optimalPressures :: M.Map BitMask Int
    }
  deriving (Eq, Show)

instance Semigroup PathOptimizer where
  (PathOptimizer m) <> (PathOptimizer m') = PathOptimizer (M.unionWith max m m')

instance Monoid PathOptimizer where
  mempty = PathOptimizer M.empty

buildSewer :: [Valve] -> Sewer
buildSewer valves =
  Sewer
    { nameToValve = M.fromList $ zip (map name valves) valves
    , distanceMatrix = fromAdjacencyList (buildAdjacencyList valves)
    , valveEncoding = encodeValves . map name . discardTrivialValves $ valves
    }

buildAdjacencyList :: [Valve] -> AdjacencyList ValveName
buildAdjacencyList valves = do
  valve <- valves

  [(name valve, neighbourName) | neighbourName <- edges valve]

encodeValves :: [ValveName] -> ValveEncoding
encodeValves valves = zip valves encodings
  where
    encodings = [bit i | i <- [0 ..]]

findOptimumPressure :: Sewer -> Path -> Int
findOptimumPressure sewer initialPath =
  maximum . M.elems . optimalPressures $ optimalPaths sewer initialPath

optimalPaths :: Sewer -> Path -> PathOptimizer
optimalPaths sewer initialPath =
  execWriter .
  head .
  dropWhile (not . null . fst . runWriter) . iterateM (step sewer) . return $
  [initialPath]

step :: Sewer -> [Path] -> Writer PathOptimizer [Path]
step _ [] = return []
step sewer paths = do
  let newPaths = concatMap (stepPath sewer) paths
  let candidateOptimumPaths = M.fromListWith max (map pathToEntry newPaths)

  tell (PathOptimizer candidateOptimumPaths)

  return newPaths

pathToEntry :: Path -> (BitMask, Int)
pathToEntry path = (visited path, currentPressure path)

stepPath :: Sewer -> Path -> [Path]
stepPath sewer path = do
  unvisited@(distance, _, _) <- findUnvisitedValves sewer path

  guard (remainingTime path > distance)

  return (updatePath unvisited path)

findUnvisitedValves :: Sewer -> Path -> [(Distance, BitMask, Valve)]
findUnvisitedValves sewer path = do
  (neighbourName, encoded) <- valveEncoding sewer
  neighbour <- maybeToList $ M.lookup neighbourName (nameToValve sewer)
  distance <-
    maybeToList $
    getDistance (distanceMatrix sewer) (currentValve path) neighbourName

  guard (encoded `unvisitedBy` path)

  return (distance, encoded, neighbour)

updatePath :: (Distance, BitMask, Valve) -> Path -> Path
updatePath (distance, encoded, valve) path =
  Path newCurrentValve newCurrentPressure newRemainingTime newVisited
  where
    newCurrentValve = name valve
    newCurrentPressure = currentPressure path + newRemainingTime * rate valve
    newRemainingTime = remainingTime path - distance - 1
    newVisited = encoded .|. visited path

unvisitedBy :: BitMask -> Path -> Bool
unvisitedBy encoded path = visited path .&. encoded == 0

discardTrivialValves :: [Valve] -> [Valve]
discardTrivialValves = filter ((/= 0) . rate)

solution :: IO ()
solution = do
  valves <- parseInput <$> readFile "data/Day16/input"
  let sewer = buildSewer valves
  let initialPath = Path "AA" 0 30 0
  
  print (findOptimumPressure sewer initialPath)
