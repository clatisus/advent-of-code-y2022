module Day1 (day1) where

import Data.List (sort)
import Data.List.Split (splitOn)

sortCalories :: String -> [Int]
sortCalories = reverse . sort . map (sum . map read) . splitOn [""] . lines

day1 :: IO ()
day1 = do
  calories <- sortCalories <$> readFile "puzzle-input/day1"
  putStrLn $ "part 1: " <> show (head calories)
  putStrLn $ "part 2: " <> show (sum . take 3 $ calories)
