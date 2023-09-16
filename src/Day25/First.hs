module Day25.First where

import Data.List (foldl', unfoldr)
import GHC.Arr (accum)

data SNAFUDigit
  = DoubleMinus
  | Minus
  | Zero
  | One
  | Two
  deriving (Eq, Show)
type SNAFUNumeral = [SNAFUDigit]

snafuBase :: Int
snafuBase = 5

findAnswer :: [SNAFUNumeral] -> SNAFUNumeral
findAnswer = toSNAFU . sum . map fromSNAFU

toSNAFU :: Int -> SNAFUNumeral
toSNAFU = toSNAFUAux []
  where
    toSNAFUAux acc n = case extractSNAFUDigit n of
      Nothing -> acc
      Just (digit, n') -> toSNAFUAux (digit:acc) n'

extractSNAFUDigit :: Int -> Maybe (SNAFUDigit, Int)
extractSNAFUDigit 0 = Nothing
extractSNAFUDigit n = Just (toSNAFUDigit modulo, (n - adjustedModulo) `div` snafuBase)
  where
    adjustedModulo = ((modulo + 2) `mod` snafuBase) - 2 -- 0..5 -> -2..2
    modulo = n `mod` snafuBase

toSNAFUDigit :: Int -> SNAFUDigit
toSNAFUDigit n = [Zero, One, Two, DoubleMinus, Minus] !! (n `mod` snafuBase)

fromSNAFU :: SNAFUNumeral -> Int
fromSNAFU = foldl' (\acc currentDigit -> snafuBase * acc + fromSNAFUDigit currentDigit) 0

fromSNAFUDigit :: SNAFUDigit -> Int
fromSNAFUDigit DoubleMinus = -2
fromSNAFUDigit Minus = -1
fromSNAFUDigit Zero = 0
fromSNAFUDigit One = 1
fromSNAFUDigit Two = 2

showSNAFUNumeral :: SNAFUNumeral -> String
showSNAFUNumeral = map showSNAFUDigit

showSNAFUDigit :: SNAFUDigit -> Char
showSNAFUDigit DoubleMinus = '='
showSNAFUDigit Minus = '-'
showSNAFUDigit Zero = '0'
showSNAFUDigit One = '1'
showSNAFUDigit Two = '2'

parseInput :: String -> [SNAFUNumeral]
parseInput = map parseNumeral . lines

parseNumeral :: String -> SNAFUNumeral
parseNumeral = map parseDigit

parseDigit :: Char -> SNAFUDigit
parseDigit '=' = DoubleMinus
parseDigit '-' = Minus
parseDigit '0' = Zero
parseDigit '1' = One
parseDigit '2' = Two

solution :: IO ()
solution = do
  numerals <- parseInput <$> readFile "data/Day25/input"
  let answer = findAnswer numerals

  print (showSNAFUNumeral answer)
