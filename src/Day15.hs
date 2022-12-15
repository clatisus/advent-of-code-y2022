{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

module Day15 (day15) where

import Control.Applicative ((<|>))
import Control.Lens (none)
import Data.Foldable (find, foldl')
import Data.List (nub, sort)
import Data.Maybe (fromJust)
import qualified Text.Parsec as P

type Point = (Int, Int)

type SensorAndBeacon = (Point, Point)

dis :: Point -> Point -> Int
dis (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parseSensors :: P.SourceName -> String -> Either P.ParseError [SensorAndBeacon]
parseSensors = P.parse $ parseSensor `P.endBy` P.newline <* P.eof
  where
    parseInt :: P.Parsec String () Int
    parseInt = (negate <$> (P.char '-' *> (read <$> P.many1 P.digit))) P.<|> (read <$> P.many1 P.digit)

    parseSensor :: P.Parsec String () SensorAndBeacon
    parseSensor = do
      x <- P.string "Sensor at x=" *> parseInt
      y <- P.string ", y=" *> parseInt
      x' <- P.string ": closest beacon is at x=" *> parseInt
      y' <- P.string ", y=" *> parseInt
      return ((x, y), (x', y'))

merge :: [(Int, Int)] -> [(Int, Int)]
merge = reverse . foldl' f [] . sort
  where
    f [] x = [x]
    f (b@(x', y') : bs) a@(x, y)
      | x <= y' + 1 = (x', max y y') : bs
      | otherwise = a : b : bs

findImpossibleX :: Int -> [SensorAndBeacon] -> [(Int, Int)]
findImpossibleX targetY = merge . filter (uncurry (<=)) . (findImpossibleX' <$>)
  where
    -- \| targetX - x | + | targetY - y | <= dis
    findImpossibleX' :: SensorAndBeacon -> (Int, Int)
    findImpossibleX' ((x, y), beacon)
      | d < 0 = (0, -1)
      | otherwise = (x - d, x + d)
      where
        d = dis (x, y) beacon - abs (targetY - y)

part1 :: [SensorAndBeacon] -> Int
part1 sab =
  subtract beaconAtTargetY
    . sum
    . map (\(x, y) -> y - x + 1)
    . findImpossibleX targetY
    $ sab
  where
    targetY = 2_000_000
    beaconAtTargetY = length . nub . filter ((== targetY) . snd) . (snd <$>) $ sab

part2 :: [SensorAndBeacon] -> Int
part2 sab =
  (\(x, y) -> x * maxCoord + y)
    . fromJust
    . foldl' (<|>) Nothing
    . ((`findPossible` sab) <$>)
    $ [0 .. maxCoord]
  where
    maxCoord = 4_000_000

    findPossible :: Int -> [SensorAndBeacon] -> Maybe (Int, Int)
    findPossible targetY sab =
      (,targetY) <$> case findImpossibleX targetY sab of
        [] -> Just 0
        ((x, y) : _)
          | 0 < x -> Just 0
          | y < maxCoord -> Just (y + 1)
          | otherwise -> Nothing

day15 :: IO ()
day15 = do
  result <- parseSensors "day15" <$> readFile "puzzle-input/day15"
  case result of
    Left err -> print err
    Right sensors -> do
      putStrLn $ "part 1: " <> show (part1 sensors)
      putStrLn $ "part 2: " <> show (part2 sensors)
