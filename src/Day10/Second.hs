module Day10.Second
  ( module Day10.Second
  , parseInput
  ) where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Day10.First (Instruction, Time, parseInput, recordEvents)

data CRTSpecs =
  CRTSpecs
    { width :: Int
    , height :: Int
    }

showCRT :: CRTSpecs -> [Bool] -> String
showCRT (CRTSpecs width height) = intercalate "\n" . chunksOf width . take (width * height) . map showPixel

showPixel :: Bool -> Char
showPixel True = '#'
showPixel False = '.'

computePixels :: CRTSpecs -> [Instruction] -> [Bool]
computePixels (CRTSpecs width _) instructions =
  zipWith isSpriteOverPixel values (concat . repeat $ [0 .. width - 1])
  where
    values = expandValues (recordEvents instructions)

expandValues :: [(Time, Int)] -> [Int]
expandValues [] = []
expandValues [(t, value)] = repeat value
expandValues events@((t1, value):(t2, _):_) = replicate (t2 - t1) value ++ expandValues (tail events)

isSpriteOverPixel :: Int -> Int -> Bool
isSpriteOverPixel spritePosition currentPixel = spritePosition - 1 <= currentPixel && currentPixel <= spritePosition + 1

solution :: IO ()
solution = do
  instructions <- parseInput <$> readFile "data/Day10/input"
  let crtSpecs = CRTSpecs 40 6
  let pixels = computePixels crtSpecs instructions

  putStrLn (showCRT crtSpecs pixels)
