module Day1.First where

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split f as = prefix:split f (dropWhile f suffix)
  where
    (prefix, suffix) = break f as

parseInput :: String -> [[Int]]
parseInput = map parseElf . split (== "") . lines

parseElf :: [String] -> [Int]
parseElf = map parseCalories

parseCalories :: String -> Int
parseCalories = read

totalCalories :: [Int] -> Int
totalCalories = sum

findMostCaloriesElf :: [[Int]] -> Int
findMostCaloriesElf = maximum . map totalCalories

solution :: IO ()
solution = do
  parsedInput <- parseInput <$> readFile "data/Day1/input"

  print (findMostCaloriesElf parsedInput)
