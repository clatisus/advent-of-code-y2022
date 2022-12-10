{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Day9 (day9) where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Monad (void)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.List (scanl')
import Data.Set (Set, insert, singleton, size)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof, many1, munch1, readP_to_S, string)

data Movement = R | L | U | D deriving (Show)

data Rope where
  Rope :: [(Int, Int)] -> Rope
  deriving (Show)

pMovements :: String -> [Movement]
pMovements = concat . fst . head . filter (null . snd) . readP_to_S (many1 pMovement)
  where
    pMovement :: ReadP [Movement]
    pMovement = do
      movement <-
        choice
          [ R <$ string "R ",
            L <$ string "L ",
            U <$ string "U ",
            D <$ string "D "
          ]
      count <- read <$> munch1 isDigit
      void (char '\n') <|> eof
      return $ replicate count movement

moveHead :: Movement -> (Int, Int) -> (Int, Int)
moveHead = \case
  R -> first succ
  L -> first pred
  U -> second succ
  D -> second pred

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (hX, hY) (tX, tY)
  | disX == 2 && disY == 2 = ((hX + tX) `div` 2, (hY + tY) `div` 2)
  | disX == 2 = ((hX + tX) `div` 2, hY)
  | disY == 2 = (hX, (hY + tY) `div` 2)
  | otherwise = (tX, tY)
  where
    disX = abs (hX - tX)
    disY = abs (hY - tY)

move :: (Set (Int, Int), Rope) -> Movement -> (Set (Int, Int), Rope)
move (set, Rope (x : xs)) movement = (last tail `insert` set, Rope tail)
  where
    head = moveHead movement x
    tail = scanl' moveTail head xs

part1 :: [Movement] -> Int
part1 = size . fst . foldl' move (singleton (0, 0), Rope $ replicate 2 (0, 0))

part2 :: [Movement] -> Int
part2 = size . fst . foldl' move (singleton (0, 0), Rope $ replicate 10 (0, 0))

day9 :: IO ()
day9 = do
  input <- pMovements <$> readFile "puzzle-input/day9"
  putStrLn $ "part 1: " <> show (part1 input)
  putStrLn $ "part 2: " <> show (part2 input)
