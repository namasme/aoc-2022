module Day16.First.Internal where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))

type ValveName = String
data Valve =
  Valve
    { name :: ValveName
    , rate :: Int
    , edges :: [ValveName]
    }
  deriving (Eq, Show)

parseInput :: String -> [Valve]
parseInput = either (error . show) id . P.parse fullInput "(parsing error)"

fullInput :: P.Parsec String u [Valve]
fullInput = P.endBy valve P.newline

valve :: P.Parsec String u Valve
valve = do
  P.string "Valve "
  _name <- valveName
  P.string " has flow rate="
  _rate <- number
  P.string "; "
  (P.try $ P.string "tunnels lead to valves ") <|> (P.try $ P.string "tunnel leads to valve ")
  _edges <- P.sepBy valveName (P.string ", ")

  return (Valve _name _rate _edges)

valveName :: P.Parsec String u ValveName
valveName = P.count 2 P.upper

number :: P.Parsec String u Int
number = read <$> P.many1 P.digit
