module Day2 (day2) where

import Control.Arrow ((***))
import Control.Monad (join)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

encode :: Char -> Int
encode x
  | x == 'A' || x == 'X' = 0
  | x == 'B' || x == 'Y' = 1
  | x == 'C' || x == 'Z' = 2

outcome :: Int -> Int -> Int
outcome x y = ((y - x + 1) `mod` 3) * 3

force :: Int -> Int -> Int
force x y = (x + y - 1) `mod` 3

score :: Int -> Int -> Int
score x y = outcome x y + y + 1

day2 :: IO ()
day2 = do
  input <- map (mapTuple encode . ((,) <$> head <*> last)) . lines <$> readFile "puzzle-input/day2"
  putStrLn $ "part 1: " <> show (sum . map (uncurry score) $ input)
  putStrLn $ "part 2: " <> show (sum . map (\(x, y) -> score x (force x y)) $ input)
