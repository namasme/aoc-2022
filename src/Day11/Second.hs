module Day11.Second
  ( module Day11.Second
  , counter
  , iterateM
  , monkeyBusiness
  , parseInput
  , monkeyRound
  ) where

import Control.Monad.Writer
import qualified Data.Map as M
import Lens.Micro.Platform

import Day11.First.Internal (Modulus)
import Day11.First
  ( Item
  , Troop
  , counter
  , iterateM
  , modulus
  , monkeyBusiness
  , monkeyRound
  , parseInput
  )

itemReducer :: [Modulus] -> Item -> Item
itemReducer moduli = (`mod` product moduli)

collectModuli :: Troop -> [Modulus]
collectModuli = map (^. modulus) . M.elems

solution :: IO ()
solution = do
  troop <- parseInput <$> readFile "data/Day11/input"

  let moduli = collectModuli troop
  let rounds = iterateM (monkeyRound (itemReducer moduli)) (return troop)
  let finalRound = rounds !! 10000

  print . monkeyBusiness . execWriter $ finalRound
