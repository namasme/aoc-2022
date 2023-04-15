module Day20.Second
  ( module Day20.Second
  , parseInput
  ) where

import Data.List (elemIndex, foldl1', partition, unfoldr)
import Data.Maybe (fromJust)
import Utils.Common (iterateN)

import Day20.First
  ( Idx
  , Permutation
  , Value
  , apply
  , applyInverse
  , parseInput
  , reduceValue
  , updatePermutation
  )

decryptionKey :: Int
decryptionKey = 811589153

applyDecryptionKey :: [Value] -> [Value]
applyDecryptionKey = map (decryptionKey *)

calculatePermutation :: [Value] -> Permutation
calculatePermutation values =
  foldl (updatePermutation totalSize) [] . concat . replicate 10 $ zip [0 ..] values
  where
    totalSize = length values

findGroveCoordinates :: [Value] -> Value
findGroveCoordinates values =
  (decryptedValues !! after 1000) + (decryptedValues !! after 2000) + (decryptedValues !! after 3000)
  where
    permutation = calculatePermutation reducedValues
    reducedValues = map (reduceValue totalSize) decryptedValues
    decryptedValues = applyDecryptionKey values
    originalZeroIndex = fromJust (0 `elemIndex` values)
    finalZeroIndex = apply originalZeroIndex permutation
    after delta = applyInverse ((finalZeroIndex + delta) `mod` totalSize) permutation
    totalSize = length values

solution :: IO ()
solution = do
  values <- parseInput <$> readFile "data/Day20/input"

  print (findGroveCoordinates values)
