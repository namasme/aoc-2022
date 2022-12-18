module Day15.First.Internal where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Utils.Spatial (Point2D(..))

type Beacon = Point2D
data Sensor =
  Sensor
    { center :: Point2D
    , radius :: Int
    }
  deriving (Eq, Show)

parseInput :: String -> ([Sensor], [Beacon])
parseInput = either (error . show) id . P.parse fullInput "(parsing error)"

fullInput :: P.Parsec String u ([Sensor], [Beacon])
fullInput = unzip <$> P.endBy sensor P.newline

sensor :: P.Parsec String u (Sensor, Beacon)
sensor = do
  P.string "Sensor at x="
  centerX <- integer
  P.string ", y="
  centerY <- integer
  P.string ": closest beacon is at x="
  beaconX <- integer
  P.string ", y="
  beaconY <- integer

  let _center = Point2D centerX centerY
  let _radius = abs (centerX - beaconX) + abs (centerY - beaconY)

  return (Sensor _center _radius, Point2D beaconX beaconY)

integer :: P.Parsec String u Int
integer = read <$> (number <|> (P.string "-" <> number))

number :: P.Parsec String u String
number = P.many1 P.digit
