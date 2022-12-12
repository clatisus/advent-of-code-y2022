{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day12 (day12) where

import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Vector as V

type Point = (Int, Int)

type Queue = S.Seq (Point, Int)

type Grid = V.Vector (V.Vector Char)

height :: Grid -> Point -> Maybe Char
height g (x, y) =
  g V.!? x
    >>= (V.!? y)
    >>= (\case 'S' -> Just 'a'; 'E' -> Just 'z'; x -> Just x)

find :: Grid -> Char -> [Point]
find g c =
  [ (x, y)
    | x <- [0 .. V.length g - 1],
      y <- [0 .. V.length (g V.! x) - 1],
      g V.! x V.! y == c
  ]

bfs :: Grid -> Point -> Queue -> Set.Set Point -> Int
bfs g end ((now@(x, y), step) S.:<| rest) visited
  | now == end = step
  | otherwise = bfs g end queue' visited'
  where
    next =
      [ adjacent
        | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)],
          let adjacent = (x + dx, y + dy),
          ((<=) <$> (g `height` adjacent) <*> (succ <$> (g `height` now))) == Just True,
          adjacent `Set.notMember` visited
      ]
    queue' = rest S.>< S.fromList ((,step + 1) <$> next)
    visited' = foldr Set.insert visited next

part1 :: Grid -> Int
part1 g = bfs g end (S.singleton (start, 0)) (Set.singleton start)
  where
    start = head $ find g 'S'
    end = head $ find g 'E'

part2 :: Grid -> Int
part2 g = bfs g end (S.fromList ((,0) <$> starts)) (Set.fromList starts)
  where
    starts = find g 'S' ++ find g 'a'
    end = head $ find g 'E'

day12 :: IO ()
day12 = do
  grid <- V.fromList . (V.fromList <$>) . lines <$> readFile "puzzle-input/day12"
  putStrLn $ "part 1: " <> show (part1 grid)
  putStrLn $ "part 2: " <> show (part2 grid)
