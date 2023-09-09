module Day24.Second (module Day24.Second) where

import Day24.First
  ( Valley
  , neighbours
  , parseInput
  , valleyEntrance
  , valleyExit
  )
import Utils.Graphs (AStarManifest(..), AStarResult(..), runAStar)
import Utils.Spatial (manhattanDistance, vonNeumannNeighbours)

findShortestPathThereAndBackAndThereAgain :: Valley -> Int
findShortestPathThereAndBackAndThereAgain valley =
  thereTime + backTime + thereAgainTime
  where
    thereAgainTime = distanceTravelled (runAStar thereAgainManifest)
    thereAgainManifest =
      buildManifest
        (thereTime + backTime, valleyEntrance valley)
        (valleyExit valley)
    backTime = distanceTravelled (runAStar backManifest)
    backManifest =
      buildManifest (thereTime, valleyExit valley) (valleyEntrance valley)
    thereTime = distanceTravelled (runAStar thereManifest)
    thereManifest = buildManifest (0, valleyEntrance valley) (valleyExit valley)
    buildManifest origin goal =
      AStarManifest
        origin
        (uncurry (neighbours valley))
        ((== goal) . snd)
        heuristic
    origin = (0, valleyEntrance valley)
    isGoal = (== valleyExit valley) . snd
    heuristic = manhattanDistance (valleyExit valley) . snd

solution :: IO ()
solution = do
  valley <- parseInput <$> readFile "data/Day24/input"

  print (findShortestPathThereAndBackAndThereAgain valley)
