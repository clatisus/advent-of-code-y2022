module Day8 (day8) where

import Data.List (foldl', transpose)
import Data.List.Split (chunksOf)
import Data.Set (Set, empty, insert, size)

-- part 1

exec :: [[(Int, Char)]] -> Set Int -> Set Int
exec grid set = foldr execr (foldr execl set grid) grid
  where
    f :: (Set Int, Char) -> (Int, Char) -> (Set Int, Char)
    f (set, acc) (i, c) = (if acc < c then i `insert` set else set, max acc c)

    execl :: [(Int, Char)] -> Set Int -> Set Int
    execl line set = fst $ foldl' f (set, '/') line

    execr :: [(Int, Char)] -> Set Int -> Set Int
    execr line set = fst $ foldr (flip f) (set, '/') line

part1 :: [[(Int, Char)]] -> Int
part1 grid = size $ exec (transpose grid) (exec grid empty)

-- part 2

scenicScore :: [String] -> Int -> Int -> (Int, Int) -> Int
scenicScore grid n m (x, y) =
  product $ scenicScore' (x, y) <$> [(0, -1), (0, 1), (-1, 0), (1, 0)]
  where
    height :: Char
    height = grid !! x !! y

    scenicScore' :: (Int, Int) -> (Int, Int) -> Int
    scenicScore' (x, y) (dx, dy) = case (x + dx, y + dy) of
      (x', y') | x' < 0 || x' >= n || y' < 0 || y' >= m -> 0
      (x', y') | height > grid !! x' !! y' -> 1 + scenicScore' (x', y') (dx, dy)
      (x', y') | height == grid !! x' !! y' -> 1
      _ -> 0

part2 :: [String] -> Int -> Int -> Int
part2 grid n m = maximum $ scenicScore grid n m <$> indices
  where
    indices = (,) <$> [0 .. (n - 1)] <*> [0 .. (m - 1)]

day8 :: IO ()
day8 = do
  input <- lines <$> readFile "puzzle-input/day8"
  let n = length input
  let m = length $ head input
  let inputWithIndex = zipWith zip (chunksOf m [1 .. n * m]) input
  print $ show (part1 inputWithIndex)
  print $ show (part2 input n m)
