{-# LANGUAGE RecordWildCards #-}

module Day4 (day4) where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP, char, eof, munch1, readP_to_S)

data RangePair = RangePair
  { l0 :: Int,
    r0 :: Int,
    l1 :: Int,
    r1 :: Int
  }

contains :: RangePair -> Bool
contains RangePair {..} = (l0 <= l1 && r1 <= r0) || (l1 <= l0 && r0 <= r1)

overlaps :: RangePair -> Bool
overlaps RangePair {..} = not (r0 < l1 || r1 < l0)

parse :: String -> RangePair
parse = fst . head . readP_to_S pRangePair
  where
    pRangePair :: ReadP RangePair
    pRangePair = RangePair <$> (pInt <* char '-') <*> (pInt <* char ',') <*> (pInt <* char '-') <*> (pInt <* eof)

    pInt :: ReadP Int
    pInt = read <$> munch1 isDigit

day4 :: IO ()
day4 = do
  input <- lines <$> readFile "puzzle-input/day4"
  putStrLn $ "part 1: " <> show (length . filter contains . (parse <$>) $ input)
  putStrLn $ "part 2: " <> show (length . filter overlaps . (parse <$>) $ input)
