module Day14 (day14) where

import Data.Foldable (foldl')
import qualified Data.Map as M
import qualified Text.Parsec as P

type Point = (Int, Int)

type Cave = M.Map Point Char

parseTraces :: P.SourceName -> String -> Either P.ParseError [[Point]]
parseTraces = P.parse $ parseTrace `P.endBy1` P.newline
  where
    parseInt = read <$> P.many1 P.digit
    parseTrace = ((,) <$> (parseInt <* P.char ',') <*> parseInt) `P.sepBy1` P.string " -> "

buildCave :: [[Point]] -> Cave
buildCave = foldl' buildCave' M.empty
  where
    buildCave' :: Cave -> [Point] -> Cave
    buildCave' cave [_] = cave
    buildCave' cave ((x, y) : p@(x', y') : rest) = buildCave' cave' (p : rest)
      where
        cave' =
          foldr
            (`M.insert` '#')
            cave
            [(x, y) | x <- [min x x' .. max x x'], y <- [min y y' .. max y y']]

fall :: (Point -> Bool) -> Int -> Point -> Maybe Point
fall blocked maxY (x, y)
  | blocked (x, y) = Nothing
  | y == maxY = Nothing
  | (not . blocked) (x, y + 1) = fall blocked maxY (x, y + 1)
  | (not . blocked) (x - 1, y + 1) = fall blocked maxY (x - 1, y + 1)
  | (not . blocked) (x + 1, y + 1) = fall blocked maxY (x + 1, y + 1)
  | otherwise = Just (x, y)

part1 :: Cave -> Int -> Int
part1 cave maxY = case fall (`M.member` cave) maxY (500, 0) of
  Nothing -> 0
  Just sand' -> 1 + part1 ((sand' `M.insert` 'o') cave) maxY

part2 :: Cave -> Int -> Int
part2 cave maxY = case fall blocked maxY (500, 0) of
  Nothing -> 0
  Just sand' -> 1 + part2 ((sand' `M.insert` 'o') cave) maxY
  where
    blocked (x, y) = y == maxY || (x, y) `M.member` cave

day14 :: IO ()
day14 = do
  traces <- parseTraces "day14" <$> readFile "puzzle-input/day14"
  case traces of
    Left err -> print err
    Right ts -> do
      let cave = buildCave ts
      let maxY = maximum . (snd <$>) . M.keys $ cave
      putStrLn $ "part 1: " <> show (part1 cave maxY)
      putStrLn $ "part 2: " <> show (part2 cave (maxY + 2))
