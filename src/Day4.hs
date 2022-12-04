module Day4 (day4) where

import Control.Arrow ((***))
import Control.Monad (join)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

contains :: ((Int, Int), (Int, Int)) -> Bool
contains = (||) <$> uncurry contains' <*> uncurry (flip contains')
  where
    contains' (a, b) (c, d) = a <= c && d <= b

overlaps :: ((Int, Int), (Int, Int)) -> Bool
overlaps ((a, b), (c, d)) = not (b < c || d < a)

parse :: String -> ((Int, Int), (Int, Int))
parse = mapTuple (mapTuple read . splitOn (== '-')) . splitOn (== ',')
  where
    splitOn c s = case break c s of
      (a, b) -> (a, tail b)

day4 :: IO ()
day4 = do
  input <- lines <$> readFile "puzzle-input/day4"
  print $ "part 1: " <> show (sum . (const 1 <$>) . filter contains . (parse <$>) $ input)
  print $ "part 2: " <> show (sum . (const 1 <$>) . filter overlaps . (parse <$>) $ input)
