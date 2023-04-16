{-# LANGUAGE DeriveFunctor #-}
module Day21.Second where

import qualified Data.Map as M
import Data.Map ((!))
import Utils.Common (both)
import Day21.First (Fix(Iso), ana, apply, cata)
import Day21.First.Internal (Job(..), Monkey, parseInput)

data Job' a
  = Add' a a
  | Subtract' a a
  | Product' a a
  | Division' a a
  | Literal' Int
  | Human
  deriving (Eq, Functor, Show)
type Monkey' = (String, Job' String)
-- RationalFunction a b c d represents (ax + b) / (cx + d)
data RationalFunction = RationalFunction Int Int Int Int deriving (Eq, Show)

add :: RationalFunction -> RationalFunction -> RationalFunction
add (RationalFunction 0 b 0 d) (RationalFunction a' b' c' d') =
  reduce $
  RationalFunction (d * a' + b * c') (d * b' + b * d') (d * c') (d * d')
add f g@(RationalFunction 0 _ 0 _) = add g f
add _ _ = error "expected one operand to be constant"

neg :: RationalFunction -> RationalFunction
neg (RationalFunction a b c d) = RationalFunction (-a) (-b) c d

sub :: RationalFunction -> RationalFunction -> RationalFunction
sub f = add f . neg

multiply :: RationalFunction -> RationalFunction -> RationalFunction
multiply (RationalFunction 0 b 0 d) (RationalFunction a' b' c' d') =
  reduce $ RationalFunction (b * a') (b * b') (d * c') (d * d')
multiply f g@(RationalFunction 0 _ 0 _) = multiply g f
multiply _ _ = error "expected one operand to be constant"

inv :: RationalFunction -> RationalFunction
inv (RationalFunction a b c d) = RationalFunction c d a b

divide :: RationalFunction -> RationalFunction -> RationalFunction
divide f  = multiply f . inv

reduce :: RationalFunction -> RationalFunction
reduce (RationalFunction a b c d) = RationalFunction a' b' c' d'
  where
    (a', b', c', d') = (a `div` _gcd, b `div` _gcd, c `div` _gcd, d `div` _gcd)
    _gcd = gcd (gcd a b) (gcd c d)

toRationalFunction :: Int -> RationalFunction
toRationalFunction n = RationalFunction 0 n 0 1

fixMonkey :: Monkey -> Monkey'
fixMonkey ("humn", _) = ("humn", Human)
fixMonkey (name, Add a b) = (name, Add' a b)
fixMonkey (name, Subtract a b) = (name, Subtract' a b)
fixMonkey (name, Product a b) = (name, Product' a b)
fixMonkey (name, Division a b) = (name, Division' a b)
fixMonkey (name, Literal a) = (name, Literal' a)

solveEquation :: RationalFunction -> RationalFunction -> Int
solveEquation (RationalFunction a b c d) (RationalFunction 0 b' 0 d') =
  (b' * d - d' * b) `div` (a * d' - c * b')
solveEquation f@(RationalFunction 0 b 0 d) g = solveEquation g f
solveEquation _ _ = error "unsolvable equation"

mainSubtrees :: Fix Job' -> (Fix Job', Fix Job')
mainSubtrees (Iso (Add' a b)) = (a, b)
mainSubtrees (Iso (Subtract' a b)) = (a, b)
mainSubtrees (Iso (Product' a b)) = (a, b)
mainSubtrees (Iso (Division' a b)) = (a, b)
mainSubtrees _ = error "expected a non-degenerate tree"

apply' :: Job' RationalFunction -> RationalFunction
apply' (Add' f g) = add f g
apply' (Subtract' f g) = sub f g
apply' (Product' f g) = multiply f g
apply' (Division' f g) = divide f g
apply' (Literal' a) = toRationalFunction a
apply' Human = RationalFunction 1 0 0 1

answer :: [Monkey] -> Int
answer monkeys =
  uncurry solveEquation . both (cata apply') . mainSubtrees . ana (monkeyMap !) $
  "root"
  where
    monkeyMap = M.fromList (map fixMonkey monkeys)

solution :: IO ()
solution = do
  parsedInput <- parseInput <$> readFile "data/Day21/input"

  print (answer parsedInput)
