module Day5.First.Internal where

import Data.List (transpose)
import Data.Maybe (catMaybes)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))

type Crate = Char
type Stack = [Crate]
data Instruction =
  Instruction
    { quantity :: Int
    , from :: Int
    , to :: Int
    }
  deriving (Eq, Show)

parseInput :: String -> ([Stack], [Instruction])
parseInput = either (error . show) id . P.parse fullInput "(parsing error)"

fullInput :: P.Parsec String u ([Stack], [Instruction])
fullInput = do
  _stacks <- stacks
  P.manyTill P.anyChar P.newline -- stack indices row
  P.newline
  _instructions <- instructions

  return (_stacks, _instructions)

stacks :: P.Parsec String u [Stack]
stacks = do
  -- wrap with P.try so that it stops at the indices row
  rows <- P.endBy1 (P.try stackRow) P.newline

  return . map catMaybes . transpose $ rows

instructions :: P.Parsec String u [Instruction]
instructions = P.endBy1 instruction P.newline

instruction :: P.Parsec String u Instruction
instruction = do
  P.string "move"
  P.space

  _quantity <- number

  P.space
  P.string "from"
  P.space

  _from <- number

  P.space
  P.string "to"
  P.space

  _to <- number

  return (Instruction _quantity _from _to)


stackRow :: P.Parsec String u [Maybe Crate]
stackRow = map toMaybeCrate <$> P.sepBy1 (crate <|> noCrate) (P.char ' ') -- P.space would accept newlines and produce a single row

toMaybeCrate :: Char -> Maybe Crate
toMaybeCrate ' ' = Nothing
toMaybeCrate c = Just c

number :: P.Parsec String u Int
number = read <$> P.many1 P.digit

crate :: P.Parsec String u Crate
crate = P.between (P.char '[') (P.char ']') P.upper

noCrate :: P.Parsec String u Char
noCrate = P.space >> P.space >> P.space
