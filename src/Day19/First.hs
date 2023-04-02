module Day19.First where

import Debug.Trace
import Control.Applicative (liftA2)
import Control.Monad.Extra (concatMapM)
import Control.Monad.Writer (Writer(..), execWriter, guard, tell)
import Data.List (transpose)
import Data.Maybe ( maybeToList )
import Data.Char (isDigit)

type Time = Int
type Cost = [Int]
type Blueprint = [Cost]

data State =
  State
    { resources :: [Int]
    , robots :: [Int]
    , elapsedTime :: Time
    }
  deriving (Eq, Show)

newtype NonNegative =
  NonNegative
    { value :: Int
    }
instance Semigroup NonNegative where
  (NonNegative n) <> (NonNegative n') = NonNegative (max n n')

instance Monoid NonNegative where
  mempty = NonNegative 0

initialState :: State
initialState =
  State {resources = [0, 0, 0, 0], robots = [1, 0, 0, 0], elapsedTime = 0}

totalQualityLevel :: Time -> [Blueprint] -> Int
totalQualityLevel timeLimit =
  sum . zipWith (*) [1 ..] . map (findMaximumGeode timeLimit initialState)

findMaximumGeode :: Time -> State -> Blueprint -> Int
findMaximumGeode timeLimit original blueprint =
  value . execWriter $ findMaximumGeodeAux blueprint timeLimit original

findMaximumGeodeAux :: Blueprint -> Time -> State -> Writer NonNegative [State]
findMaximumGeodeAux blueprint timeLimit original = do
  let nexts = nextStates blueprint timeLimit original
  let remainingTime = timeLimit - elapsedTime original
  case nexts of
    [] ->
      (tell . NonNegative . getGeode . evolve remainingTime $ original) >>
      return []
    _ -> concatMapM (findMaximumGeodeAux blueprint timeLimit) nexts

getGeode :: State -> Int
getGeode state = resources state !! 3

evolve :: Time -> State -> State
evolve duration original =
  original {resources = newResources, elapsedTime = newElapsedTime}
  where
    newResources =
      zipWith (+) (resources original) $ map (duration *) (robots original)
    newElapsedTime = elapsedTime original + duration

nextStates :: Blueprint -> Time -> State -> [State]
nextStates blueprint timeLimit original = do
  (robotIdx, cost) <- zip [0 ..] blueprint

  let requiredTimes =
        zipWith3 requiredTime cost (robots original) (resources original)
  let mState =
        spawnRobot blueprint robotIdx original <$>
        foldl1 (liftA2 max) requiredTimes

  state <- maybeToList mState

  guard (elapsedTime state <= timeLimit)
  guard (not $ isOverkill blueprint state)

  return state

isOverkill :: Blueprint -> State -> Bool
isOverkill blueprint state =
  or . take 3 . zipWith (>) (robots state) . map maximum . transpose $ blueprint

requiredTime :: Int -> Int -> Int -> Maybe Time
requiredTime 0 _ _ = Just 0
requiredTime _ 0 _ = Nothing
requiredTime cost availableRobots stock
  | stock >= cost = Just 0
  | otherwise =
    Just $ ceiling (fromIntegral (cost - stock) / fromIntegral availableRobots)

spawnRobot :: Blueprint -> Int -> State -> Time -> State
spawnRobot blueprint robotIdx original requiredTime =
  evolved
    { robots = incrementAt robotIdx (robots original)
    , resources = zipWith (-) (resources evolved) (blueprint !! robotIdx)
    }
  where
    evolved = evolve (requiredTime + 1) original

incrementAt :: Enum a => Int -> [a] -> [a]
incrementAt _ [] = error ""
incrementAt 0 (x:xs) = succ x : xs
incrementAt n (x:xs) = x : incrementAt (n - 1) xs

parseInput :: String -> [Blueprint]
parseInput = map parseBlueprint . lines

parseBlueprint :: String -> Blueprint
parseBlueprint line =
  [ [oreOre, 0, 0, 0]
  , [oreClay, 0, 0, 0]
  , [oreObsidian, clayObsidian, 0, 0]
  , [oreGeode, 0, obsidianGeode, 0]
  ]
  where
    [oreOre, oreClay, oreObsidian, clayObsidian, oreGeode, obsidianGeode] =
      map read . filter (all isDigit) . words $ line

solution :: IO ()
solution = do
  blueprints <- parseInput <$> readFile "data/Day19/input"
  let timeLimit = 24

  print (totalQualityLevel timeLimit blueprints)
