{-# LANGUAGE TemplateHaskell #-}

module Day11.First.Internal where

import Lens.Micro.Platform
import qualified Data.Map as M
import qualified Text.Parsec as P
import Text.Parsec ((<|>))

type MonkeyID = Int
type Item = Integer
type Modulus = Integer
data Monkey =
  Monkey
    { _items :: [Item]
    , _inspect :: Item -> Item
    , _modulus :: Modulus
    , _destinationIds :: (MonkeyID, MonkeyID)
     --_inspectAndThrow :: Item -> (MonkeyID, Item)
    }
type Troop = M.Map MonkeyID Monkey

$(makeLenses ''Monkey)

instance Show Monkey where
  show (Monkey items _ _ _) = show items

parseInput :: String -> Troop
parseInput = either (error . show) id . P.parse fullInput "(parsing error)"

fullInput :: P.Parsec String u Troop
fullInput = M.fromList . zip [0 ..] <$> P.sepBy monkey P.newline

monkey :: P.Parsec String u Monkey
monkey = do
  header
  P.newline
  _items <- startingItems
  P.newline
  _operation <- operation
  P.newline
  (_modulus, _destinationIds) <- test
  P.newline

  return (Monkey _items _operation _modulus _destinationIds)

header :: P.Parsec String u Char
header = P.string "Monkey " >> number >> P.char ':'

startingItems :: P.Parsec String u [Item]
startingItems = do
  P.string "  Starting items: "
  P.sepBy (toInteger <$> number) (P.string ", ")

operation :: P.Parsec String u (Item -> Item)
operation = do
  P.string "  Operation: new = old "
  _operator <- operator
  P.space
  _rightArgument <- Left . toInteger <$> number <|> Right <$> P.string "old"

  case _rightArgument of
    Left n -> return (`_operator` n)
    Right _ -> return (\x -> _operator x x)

test :: P.Parsec String u (Modulus, (MonkeyID, MonkeyID))
test = do
  P.string "  Test: divisible by "
  _modulus <- toInteger <$> number

  P.newline
  P.string "    If true: throw to monkey "
  ifTrue <- number

  P.newline
  P.string "    If false: throw to monkey "
  ifFalse <- number

  return (_modulus, (ifTrue, ifFalse))

operator :: P.Parsec String u (Item -> Item -> Item)
operator = do
  _operator <- Left <$> P.char '+' <|> Right <$> P.char '*'

  case _operator of
    Left _ -> return (+)
    Right _ -> return (*)

number :: P.Parsec String u Int
number = read <$> P.many1 P.digit
