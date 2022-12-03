module Day3 where

import Data.Char (isLower, isUpper, ord)
import Data.List (find)
import Data.Set (fromList, member)

getPriority :: Maybe Char -> Int
getPriority (Just x)
  | isLower x = ord x - ord 'a' + 1
  | otherwise = ord x - ord 'A' + 27
getPriority Nothing = 0

findDup :: (String, String) -> Maybe Char
findDup (x, y) = find (`member` set) y
  where
    set = fromList x

findDup3 :: (String, String, String) -> Maybe Char
findDup3 (x, y, z) = find (`member` setX) $ filter (`member` setY) z
  where
    setX = fromList x
    setY = fromList y

splitHalf :: String -> (String, String)
splitHalf x = splitAt (length x `div` 2) x

eachThree :: [String] -> [(String, String, String)]
eachThree [] = []
eachThree (x : y : z : xs) = (x, y, z) : eachThree xs

day3 :: IO ()
day3 = do
  input <- lines <$> readFile "puzzle-input/day3"
  print $ "part 1: " <> show (sum $ map (getPriority . findDup . splitHalf) input)
  print $ "part 2: " <> show (sum $ map (getPriority . findDup3) $ eachThree input)
