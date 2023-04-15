module Day20.First where

import qualified Data.DList as DL
import Data.List (elemIndex, foldl', partition, unfoldr)
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe (fromJust)
import qualified Data.Set as S

import Utils.Common (both)

type Idx = Int
type Value = Int
type Chunk = ([Idx], Idx)
type ChunkMap = M.Map Idx Chunk
data OSet =
  OSet
    { ordered :: [Idx]
    , elements :: S.Set Idx
    } deriving (Eq, Show)
type Cycle = OSet
type Permutation = [Cycle]


(|/\) :: OSet -> OSet -> S.Set Int
(OSet _ self) |/\ (OSet _ other) = S.intersection self other

size :: OSet -> Int
size = S.size . elements

member :: Int -> OSet -> Bool
member x oset = x `S.member` elements oset

toCycle :: [Idx] -> Cycle
toCycle values = OSet {ordered = values, elements = S.fromList values}

snoc :: Cycle -> Permutation -> Permutation
snoc c [] = [c]
snoc c cs =
  disjoint ++ filter (\cycle -> size cycle >= 2) (snocAux c coincident common)
  where
    (disjoint, coincident, common) = partitionCycles c cs

snocAux :: Cycle -> Permutation -> S.Set Idx -> Permutation
snocAux c [] _ = [c]
snocAux c cs common = assembleChunks chunks chunks'
  where
    chunks = foldl' M.union M.empty . map (chunkify common . ordered) $ cs
    chunks' = chunkify common (ordered c)

partitionCycles :: Cycle -> Permutation -> (Permutation, Permutation, S.Set Idx)
partitionCycles c cs = (disjoint, coincident, common)
  where
    (disjoint, coincident) = both (map fst) partitions
    common = foldl' S.union S.empty . map snd . snd $ partitions
    partitions = partition (S.null . snd) intersections
    intersections = zip cs . map (c |/\) $ cs

assembleChunks :: ChunkMap -> ChunkMap -> [Cycle]
assembleChunks chunks chunks' = unfoldr extractCycle (chunks, chunks')

extractCycle :: (ChunkMap, ChunkMap) -> Maybe (Cycle, (ChunkMap, ChunkMap))
extractCycle (chunks, chunks')
  | M.null chunks && M.null chunks' = Nothing
  | otherwise = Just (cycle, (newChunks, newChunks'))
  where
    (newChunks, newChunks', cycle) = extractCycleAux chunks chunks' seed DL.empty
    seed = head (M.keys chunks)

extractCycleAux :: ChunkMap -> ChunkMap -> Idx -> DL.DList Idx -> (ChunkMap, ChunkMap, Cycle)
extractCycleAux chunks chunks' seed lump =
  case followSeed (chunks, chunks', seed) of
    Nothing -> (chunks, chunks', toCycle $ DL.toList lump)
    Just (component, (newChunks, newChunks', next)) ->
      extractCycleAux newChunks newChunks' next (DL.append lump (DL.fromList component))

followSeed :: (ChunkMap, ChunkMap, Idx) -> Maybe ([Idx], (ChunkMap, ChunkMap, Idx))
followSeed (chunks, chunks', seed)
  | M.notMember seed chunks = Nothing
  | otherwise =
    Just (seed : component ++ component', (newChunks, newChunks', next))
  where
    newChunks = M.delete seed chunks
    newChunks' = M.delete seed' chunks'
    (component, seed') = chunks ! seed
    (component', next) = chunks' ! seed'

chunkify :: S.Set Idx -> [Idx] -> ChunkMap
chunkify common cycle = M.fromList . toChunks $ chunkifyAux common prefix suffix
  where
    (prefix, suffix) = break (`S.member` common) cycle

toChunks :: [(Idx, [Idx])] -> [(Idx, Chunk)]
toChunks [] = []
toChunks preChunks@((seed, _):_) = toChunksAux seed preChunks

toChunksAux :: Idx -> [(Idx, [Idx])] -> [(Idx, Chunk)]
toChunksAux _ [] = []
toChunksAux seed [(next, component)] = [(next, (component, seed))]
toChunksAux seed ((next, component):(next', component'):rest) =
  (next, (component, next')) : toChunksAux seed ((next', component') : rest)

chunkifyAux :: S.Set Idx -> [Idx] -> [Idx] -> [(Idx, [Idx])]
chunkifyAux _ _ [] = []
chunkifyAux common prefix (current:remainder)
  | null newSuffix = [(current, newPrefix ++ prefix)]
  | otherwise = (current, newPrefix) : chunkifyAux common prefix newSuffix
  where
    (newPrefix, newSuffix) = break (`S.member` common) remainder

apply :: Idx -> Permutation -> Idx
apply x [] = x
apply x (c:cs) = case elemIndex x (ordered c) of
  Nothing -> apply x cs
  Just i -> ordered c !! ((i + 1) `mod` size c)

applyInverse :: Idx -> Permutation -> Idx
applyInverse x [] = x
applyInverse x (c:cs) = case elemIndex x (ordered c) of
  Nothing -> applyInverse x cs
  Just i -> ordered c !! ((i - 1) `mod` size c)

calculatePermutation :: [Value] -> Permutation
calculatePermutation values =
  foldl (updatePermutation totalSize) [] (zip [0 ..] values)
  where
    totalSize = length values

updatePermutation :: Int -> Permutation -> (Idx, Value) -> Permutation
updatePermutation totalSize permutation (originalIndex, value) = snoc (buildCycle newIndex end) permutation
  where
    newIndex = apply originalIndex permutation
    end = (newIndex + value) `mod` (totalSize - 1)

buildCycle :: Idx -> Idx -> Cycle
buildCycle start end
  | start == end = toCycle [start]
  | otherwise = toCycle $ enumFromThenTo end (end + signum (start - end)) start

findGroveCoordinates :: [Value] -> Value
findGroveCoordinates values =
  (values !! after 1000) + (values !! after 2000) + (values !! after 3000)
  where
    permutation = calculatePermutation reducedValues
    reducedValues = map (reduceValue totalSize) values
    originalZeroIndex = fromJust (0 `elemIndex` values)
    finalZeroIndex = apply originalZeroIndex permutation
    after delta = applyInverse ((finalZeroIndex + delta) `mod` length values) permutation
    totalSize = length values

reduceValue :: Int -> Value -> Value
reduceValue totalSize value = value `mod` (totalSize - 1)

parseInput :: String -> [Int]
parseInput = map read . lines

solution :: IO ()
solution = do
  values <- parseInput <$> readFile "data/Day20/input"

  print (findGroveCoordinates values)
