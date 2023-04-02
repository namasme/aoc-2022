module Day19.Second
  ( module Day19.Second
  , State(..)
  , initialState
  , parseInput
  ) where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Control.Monad.Extra (concatMapM)
import Data.List (transpose)
import Data.Maybe ( maybeToList )
import Data.Char (isDigit)
import Day19.First
  ( Blueprint
  , State(..)
  , Time
  , getGeode
  , initialState
  , isOverkill
  , nextStates
  , parseInput
  , requiredTime
  , spawnRobot
  )


calculateAnswer :: Time -> [Blueprint] -> Int
calculateAnswer timeLimit = product . map (findMaximumGeode timeLimit initialState) . take 3

findMaximumGeode :: Time -> State -> Blueprint -> Int
findMaximumGeode timeLimit original blueprint =
  findMaximumGeodeAux blueprint timeLimit 0 [original]

findMaximumGeodeAux :: Blueprint -> Time -> Int -> [State] -> Int
findMaximumGeodeAux _ _ securedGeode [] = securedGeode
findMaximumGeodeAux blueprint timeLimit securedGeode candidates = findMaximumGeodeAux blueprint timeLimit newSecuredGeode filteredNewCandidates
  where
    newSecuredGeode = maximum . (securedGeode:) . map (minimumExpectedGeode timeLimit) $ newCandidates
    filteredNewCandidates = filter (isWorth timeLimit newSecuredGeode) newCandidates
    newCandidates = concatMap (nextStates blueprint timeLimit) candidates

isWorth :: Time -> Int -> State -> Bool
isWorth timeLimit securedGeode state = maximumExpectedGeode timeLimit state > securedGeode

minimumExpectedGeode :: Time -> State -> Int
minimumExpectedGeode timeLimit state =
  getGeode state + (timeLimit - elapsedTime state) * (robots state !! 3)

maximumExpectedGeode :: Time -> State -> Int
maximumExpectedGeode timeLimit state = getGeode state + producedGeode
  where
    remainingTime = timeLimit - elapsedTime state
    currentGeodeRobots = robots state !! 3
    finalGeodeRobots = currentGeodeRobots + remainingTime
    producedGeode = ((finalGeodeRobots - 1) * finalGeodeRobots - (currentGeodeRobots - 1) * currentGeodeRobots) `div` 2

solution :: IO ()
solution = do
  blueprints <- parseInput <$> readFile "data/Day19/input"
  let timeLimit = 32

  print (calculateAnswer timeLimit blueprints)
