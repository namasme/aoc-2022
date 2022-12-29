module Day16.Second
  ( module Day16.Second
  , Path(..)
  , buildSewer
  , parseInput
  ) where

import Control.Monad (guard)
import Data.Bits ((.&.))
import qualified Data.Map as M
import Day16.First
  ( BitMask
  , Path(..)
  , Sewer
  , Valve(..)
  , ValveName
  , buildSewer
  , optimalPaths
  , optimalPressures
  , parseInput
  , step
  )

type ValvesBitMaskMap = M.Map ValveName BitMask

areDisjoint :: BitMask -> BitMask -> Bool
areDisjoint v1 v2 = v1 .&. v2 == 0

findOptimumPressure :: Sewer -> Path -> Int
findOptimumPressure sewer initialPath =
   optimumPair . M.toList . optimalPressures $ optimalPaths sewer initialPath

optimumPair :: [(BitMask, Int)] -> Int
optimumPair paths = maximum $ do
  (_visited, score) <- paths
  (_visited', score') <- paths

  guard (areDisjoint _visited _visited') -- no open valves in common

  return (score + score')

solution :: IO ()
solution = do
  valves <- parseInput <$> readFile "data/Day16/input"
  let sewer = buildSewer valves
  let initialPath = Path "AA" 0 26 0

  print (findOptimumPressure sewer initialPath)
