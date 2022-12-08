module Day7.First
  ( module Day7.First
  , Directory(..)
  , File(..)
  , parseInput
  ) where

import Day7.First.Internal (Directory(..), File(..), parseInput)

directorySize :: Directory -> Int
directorySize d = sum (map size (files d)) + sum (map directorySize (directories d))

allDirectories :: Directory -> [Directory]
allDirectories d = d : concatMap allDirectories (directories d)

findSmallDirectories :: Int -> Directory -> [Directory]
findSmallDirectories maxSize = filter ((<= maxSize) . directorySize) . allDirectories

totalSmallDirectoriesSize :: Int -> Directory -> Int
totalSmallDirectoriesSize maxSize = sum . map directorySize . findSmallDirectories maxSize

solution :: IO ()
solution = do
  root <- parseInput <$> readFile "data/Day7/input"
  let maxSize = 100000

  print (totalSmallDirectoriesSize 100000 root)
