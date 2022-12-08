module Day7.Second
  ( module Day7.Second
  , Directory(..)
  , File(..)
  , parseInput
  ) where

import Day7.First
  ( Directory(..)
  , File(..)
  , allDirectories
  , directorySize
  , parseInput
  )

findOptimalDirectorySize :: Int -> Int -> Directory -> Int
findOptimalDirectorySize totalAvailableSpace requiredSpace root =
  minimum . filter (>= minSize) . map directorySize . allDirectories $ root
  where
    minSize = requiredSpace - (totalAvailableSpace - directorySize root)

solution :: IO ()
solution = do
  root <- parseInput <$> readFile "data/Day7/input"
  let totalAvailableSpace = 70000000
  let requiredSpace = 30000000

  print (findOptimalDirectorySize totalAvailableSpace requiredSpace root)
