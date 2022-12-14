module Day14 (day14) where

import Control.Monad (mfilter)
import Data.Foldable (find, foldl')
import Data.List (group)
import Data.List.Extras.LazyLength (lengthBound)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import qualified Text.Parsec as P

newtype Trace = Trace [(Int, Int)] deriving (Show)

type Cave = M.Map (Int, Int) Char

parseTraces :: P.SourceName -> String -> Either P.ParseError [Trace]
parseTraces = P.parse $ parseTrace `P.endBy1` P.newline
  where
    parseInt :: P.Parsec String () Int
    parseInt = read <$> P.many1 P.digit

    parseTrace :: P.Parsec String () Trace
    parseTrace =
      Trace
        <$> (((,) <$> (parseInt <* P.char ',') <*> parseInt) `P.sepBy1` P.string " -> ")

buildCave :: [Trace] -> Cave
buildCave = foldl' buildCave' M.empty
  where
    buildCave' :: Cave -> Trace -> Cave
    buildCave' cave (Trace [_]) = cave
    buildCave' cave (Trace ((x, y) : p@(x', y') : rest)) = buildCave' cave' (Trace (p : rest))
      where
        cave' :: Cave
        cave' =
          foldr
            (`M.insert` '#')
            cave
            [(x, y) | x <- [min x x' .. max x x'], y <- [min y y' .. max y y']]

fall :: ((Int, Int) -> Bool) -> ((Int, Int) -> Bool) -> (Int, Int) -> Maybe (Int, Int)
fall blocked endlessVoid (x, y) =
  (head <$>) . find (lengthBound 1 (<)) . group . takeWhile (not . endlessVoid) $ falls
  where
    falls :: [(Int, Int)]
    falls = catMaybes . takeWhile (/= Nothing) . tail $ iterate (singleFall blocked) (Just (x, y))
      where
        singleFall :: ((Int, Int) -> Bool) -> Maybe (Int, Int) -> Maybe (Int, Int)
        singleFall _ Nothing = Nothing
        singleFall blocked (Just (x, y)) =
          find (not . blocked) [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1), (x, y)]

part1 :: Cave -> Int
part1 cave = part1' cave
  where
    maxY = maximum . (snd <$>) . M.keys $ cave
    endlessVoid = (> maxY) . snd

    part1' cave' = case fall (`M.member` cave') endlessVoid (500, 0) of
      Nothing -> 0
      Just sand' -> 1 + part1' ((sand' `M.insert` 'o') cave')

part2 :: Cave -> Int
part2 cave = part2' cave
  where
    maxY = (+ 2) . maximum . (snd <$>) . M.keys $ cave
    endlessVoid = (> maxY) . snd
    blocked cave (x, y) = y == maxY || (x, y) `M.member` cave

    part2' cave' = case fall (blocked cave') endlessVoid (500, 0) of
      Nothing -> 0
      Just sand' -> 1 + part2' ((sand' `M.insert` 'o') cave')

day14 :: IO ()
day14 = do
  traces <- parseTraces "day14" <$> readFile "puzzle-input/day14"
  case traces of
    Left err -> print err
    Right ts -> do
      let cave = buildCave ts
      putStrLn $ "part 1: " <> show (part1 cave)
      putStrLn $ "part 2: " <> show (part2 cave)
