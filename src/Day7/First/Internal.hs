module Day7.First.Internal where

import Data.Either (rights)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))

data Directory =
  Directory
    { name :: String
    , directories :: [Directory]
    , files :: [File]
    }
  deriving (Eq, Show)
data File =
  File
    { filename :: String
    , size :: Int
    }
  deriving (Eq, Show)

parseInput :: String -> Directory
parseInput = either (error . show) id . P.parse fullInput "(parsing error)"

fullInput :: P.Parsec String u Directory
fullInput = directory

directory :: P.Parsec String u Directory
directory = do
  P.string "$ cd "

  _name <- dirName

  P.newline
  P.string "$ ls"
  P.newline

  _files <- rights <$> P.many ((Left <$> dirEntry) <|> (Right <$> fileEntry))

  _directories <- P.many (P.try directory) -- don't consume in case we find a cd ..

  P.optional (P.string "$ cd .." <* P.newline)

  return (Directory _name _directories _files)

dirName :: P.Parsec String u String
dirName = P.many1 (P.letter <|> P.char '/')

fileName :: P.Parsec String u String
fileName = P.many1 (P.letter <|> P.char '.')

dirEntry :: P.Parsec String u String
dirEntry = P.string "dir " *> dirName <* P.newline

fileEntry :: P.Parsec String u File
fileEntry = do
  _size <- number
  P.space
  _filename <- fileName
  P.newline

  return (File _filename _size)

number :: P.Parsec String u Int
number = read <$> P.many1 P.digit
