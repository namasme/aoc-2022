module Day13.First.Internal where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))

data PacketValue
  = List [PacketValue]
  | Number Int
  deriving (Eq, Show)

type Packet = [PacketValue]
type PacketPair = (Packet, Packet)

parseInput :: String -> [PacketPair]
parseInput = either (error . show) id . P.parse fullInput "(parsing error)"

fullInput :: P.Parsec String u [PacketPair]
fullInput = P.sepBy packetPair P.newline

packetPair :: P.Parsec String u PacketPair
packetPair = do
  first <- packet
  P.newline
  second <- packet
  P.newline

  return (first, second)

packet :: P.Parsec String u Packet
packet = P.char '[' *> P.sepBy packetValue (P.char ',') <* P.char ']'

packetValue :: P.Parsec String u PacketValue
packetValue = (List <$> packet) <|> (Number <$> number)

number :: P.Parsec String u Int
number = read <$> P.many1 P.digit
