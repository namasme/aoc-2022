module Day15.Second
  ( module Day15.Second
  , parseInput
  ) where

import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Day15.First.Internal (Beacon, Sensor(..), parseInput)
import Utils.Spatial (Point2D(..))
import Day15.First hiding (solution)

type RowIdx = Int
type Section = [Interval]
data ScanArea =
  ScanArea
    { xMin :: Int
    , xMax :: Int
    , yMin :: Int
    , yMax :: Int
    }

tuningFrequency :: Point2D -> Int
tuningFrequency (Point2D x y) = 4000000 * x + y

findDistressBeacon :: ScanArea -> [Section] -> Point2D
findDistressBeacon scanArea sections = findMissingPoint scanArea (findIncompleteRow scanArea sections)

findIncompleteRow :: ScanArea -> [Section] -> (Section, RowIdx)
findIncompleteRow scanArea sections = head $ do
      (section, row) <- zip sections [yMin scanArea ..]
      guard (not $ containsScanArea scanArea section)

      let intersectedSection = intersectSection scanArea section

      return (intersectedSection, row)

containsScanArea :: ScanArea -> Section -> Bool
containsScanArea scanArea [] = False
containsScanArea scanArea ((xMin', xMax'):rest)
  | xMax' < xMin scanArea = containsScanArea scanArea rest
  | xMin' <= xMin scanArea && xMax' >= xMax scanArea = True
  | otherwise = False

-- This function assumes the section it receives have already been sorted,
-- merged, and intersected with the scan area. It also assumes the input is
-- correct and only one point is missing from the section.
findMissingPoint :: ScanArea -> (Section, RowIdx) -> Point2D
findMissingPoint scanArea ([(xMin', xMax')], row)
  | xMin' > xMin scanArea = Point2D (xMin' - 1) row
  | otherwise = Point2D (xMax' + 1) row
findMissingPoint scanArea ([(_, xMax'), (_, _)], row) = Point2D (xMax' + 1) row
findMissingPoint _ _ = error "Unexpected input"

intersectSection :: ScanArea -> Section -> Section
intersectSection (ScanArea _xMin _xMax _ _) = mapMaybe (intersectInterval (_xMin, _xMax))

intersectInterval :: Interval -> Interval -> Maybe Interval
intersectInterval (_xMin, _xMax) (xMin', xMax')
  | _xMin > xMin' = intersectInterval (xMin', xMax') (_xMin, _xMax)
  | _xMax >= xMin' = Just (max _xMin xMin', min _xMax xMax')
  | otherwise = Nothing

computeSections :: ScanArea -> [Sensor] -> [Section]
computeSections scanArea sensors = map (flip computeSection sensors) [yMin scanArea .. yMax scanArea]

solution :: IO ()
solution = do
  (sensors, beacons) <- parseInput <$> readFile "data/Day15/input"
  let scanArea = ScanArea 0 4000000 0 4000000
  let sections = computeSections scanArea sensors
  let distressBeacon = findDistressBeacon scanArea sections

  print (tuningFrequency distressBeacon)
