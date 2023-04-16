module Day21.First where

import qualified Data.Map as M
import Data.Map ((!))
import Day21.First.Internal (Job(..), Monkey, parseInput)

-- Adapted from https://en.wikipedia.org/wiki/Catamorphism#General_case
newtype Fix f = Iso { invIso :: f (Fix f) }

cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata alg = alg . fmap (cata alg) . invIso

ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana coalg = Iso . fmap (ana coalg) . coalg

hylo :: Functor f => (a -> f a) -> (f b -> b) -> a -> b
hylo coalg alg = cata alg . ana coalg

apply :: Job Int -> Int
apply (Add a b) = a + b
apply (Subtract a b) = a - b
apply (Product a b) = a * b
apply (Division a b) = a `div` b
apply (Literal a) = fromIntegral a

answer :: [Monkey] -> Int
answer monkeys = hylo (monkeyMap !) apply "root"
  where
    monkeyMap = M.fromList monkeys

solution :: IO ()
solution = do
  monkeys <- parseInput <$> readFile "data/Day21/input"

  print (answer monkeys)
