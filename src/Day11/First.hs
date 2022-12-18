module Day11.First
  ( module Day11.First
  , parseInput
  , modulus
  , Item
  , Troop
  ) where

import Control.Monad.Writer
import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Lens.Micro.Platform
import Utils.Common (iterateM)
import Day11.First.Internal
  ( Item
  , Monkey(..)
  , MonkeyID
  , Troop
  , destinationIds
  , inspect
  , items
  , modulus
  , parseInput
  )

newtype ThrowsCounter =
  ThrowsCounter
    { counter :: M.Map MonkeyID Int
    }

instance Semigroup ThrowsCounter where
  (ThrowsCounter m) <> (ThrowsCounter m') = ThrowsCounter (M.unionWith (+) m m')

instance Monoid ThrowsCounter where
  mempty = ThrowsCounter M.empty

data Throw =
  Throw
    { from :: MonkeyID
    , to :: MonkeyID
    , item :: Item
    }
  deriving (Eq, Show)
type ItemReducer = Item -> Item

monkeyBusiness :: ThrowsCounter -> Int
monkeyBusiness = (\[x, y] -> x * y) . take 2 . reverse . sort . M.elems . counter

monkeyRound :: ItemReducer -> Troop -> Writer ThrowsCounter Troop
monkeyRound itemReducer troop = monkeyRoundAux itemReducer troop 0

monkeyRoundAux :: ItemReducer -> Troop -> MonkeyID -> Writer ThrowsCounter Troop
monkeyRoundAux itemReducer troop currentMonkeyId
  | currentMonkeyId == M.size troop = return troop
  | otherwise = tell (ThrowsCounter $ M.singleton currentMonkeyId (length throws)) >> monkeyRoundAux itemReducer newTroop (currentMonkeyId + 1)
  where
    newTroop = backfillMonkeys troop throws & at currentMonkeyId . _Just . items .~ []
    throws = monkeyThrows itemReducer currentMonkeyId . fromJust $ troop ^. at currentMonkeyId

backfillMonkeys :: Troop -> [Throw] -> Troop
backfillMonkeys troop [] = troop
backfillMonkeys troop ((Throw sourceId destinationId _item):rest) = backfillMonkeys newTroop rest
  where
    newTroop = troop & at destinationId . _Just . items %~ (++[_item])

monkeyThrows :: ItemReducer -> MonkeyID -> Monkey -> [Throw]
monkeyThrows itemReducer sourceId monkey =
  map
    (toThrow sourceId . test . itemReducer . (monkey ^. inspect))
    (monkey ^. items)
  where
    test :: Item -> (MonkeyID, Item)
    test item =
      if item `mod` (toInteger (monkey ^. modulus)) == 0
        then (monkey ^. destinationIds . _1, item)
        else (monkey ^. destinationIds . _2, item)

toThrow :: MonkeyID -> (MonkeyID, Item) -> Throw
toThrow sourceId (destinationId, _item) = Throw sourceId destinationId _item

solution :: IO ()
solution = do
  troop <- parseInput <$> readFile "data/Day11/input"

  let itemReducer = (`div` 3)
  let rounds = iterateM (monkeyRound itemReducer) (return troop)
  let finalRound = rounds !! 20

  print . monkeyBusiness . execWriter $ finalRound
