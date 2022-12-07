module Utils.Common where

import Data.Function (on)

-- Like break but it discards the _offending_ character from the output
splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen p as = (prefix, tail suffix)
  where
    (prefix, suffix) = break p as

-- Like splitWhen but the predicate is implicit (match the provided value)
splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn a = splitWhen (==a)

-- Applies the same transformation to both members of a tuple
both :: (a -> b) -> (a, a) -> (b, b)
both f = uncurry ((,) `on` f)
