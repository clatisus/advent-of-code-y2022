module Day6 (day6) where

import Data.List (nub)

findFirstDistinctBy :: String -> Int -> Int
findFirstDistinctBy xs n =
  if distinct (take n xs)
    then n
    else 1 + findFirstDistinctBy (tail xs) n
  where
    distinct = (==) <$> length <*> (length . nub)

day6 :: IO ()
day6 = do
  input <- readFile "puzzle-input/day6"
  print $ "part 1: " <> show (findFirstDistinctBy input 4)
  print $ "part 2: " <> show (findFirstDistinctBy input 14)
