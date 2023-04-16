{-# LANGUAGE DeriveFunctor #-}
module Day21.First.Internal where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))

data Job a
  = Add a a
  | Subtract a a
  | Product a a
  | Division a a
  | Literal Int
  deriving (Eq, Functor, Show)
type Monkey = (String, Job String)

parseInput :: String -> [Monkey]
parseInput = either (error . show) id . P.parse fullInput "(parsing error)"

fullInput :: P.Parsec String u [Monkey]
fullInput = P.endBy monkey P.newline

monkey :: P.Parsec String u Monkey
monkey = do
  name <- monkeyName
  P.string ": "

  _job <- job

  return (name, _job)

monkeyName :: P.Parsec String u String
monkeyName  = P.count 4 P.lower

job :: P.Parsec String u (Job String)
job = (Literal <$> number) <|> operation

operation :: P.Parsec String u (Job String)
operation = do
  leftName <- monkeyName
  P.space
  _operator <- operator
  P.space
  rightName <- monkeyName

  return (_operator leftName rightName)

operator :: P.Parsec String u (a -> a -> Job a)
operator =
  (Add <$ P.char '+') <|> (Subtract <$ P.char '-') <|> (Product <$ P.char '*') <|>
  (Division <$ P.char '/')


number :: P.Parsec String u Int
number = read <$> P.many1 P.digit
