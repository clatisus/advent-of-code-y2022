{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Day5 (day5) where

import Control.Arrow ((***))
import Control.Lens (element, (&), (.~))
import Data.Char (isDigit, isSpace)
import Data.List (foldl', transpose)
import Text.ParserCombinators.ReadP (char, choice, eof, get, munch1, readP_to_S, sepBy1, string)

data Stack where
  Stack :: String -> Stack

data Move = Move
  { count :: Int,
    from :: Int,
    to :: Int
  }

parse :: String -> String
parse = fst . head . readP_to_S (pCrate `sepBy1` char ' ' <* eof)
  where
    pCrate = choice [char '[' *> get <* char ']', char ' ' *> get <* char ' ']

parseStacks :: [String] -> [Stack]
parseStacks = (Stack . dropWhile isSpace <$>) . transpose . (parse <$>)

parseMove :: String -> Move
parseMove = fst . head . readP_to_S pMove
  where
    pMove = do
      string "move "
      count <- read <$> munch1 isDigit
      string " from "
      from <- read <$> munch1 isDigit
      string " to "
      to <- read <$> munch1 isDigit
      eof
      return Move {..}

performMovesWith :: [Stack] -> [Move] -> (String -> String) -> [Stack]
performMovesWith stacks moves f = foldl' performMove stacks moves
  where
    performMove stacks Move {..} =
      let from' = from - 1
          to' = to - 1
          (Stack fromStack) = stacks !! from'
          (Stack toStack) = stacks !! to'
          (toBeMoved, rest) = splitAt count fromStack
       in stacks & (element from' .~ Stack rest) & (element to' .~ Stack (f toBeMoved <> toStack))

day5 :: IO ()
day5 = do
  input <- lines <$> readFile "puzzle-input/day5"
  let (stacks, moves) = (parseStacks *** ((parseMove <$>) . tail)) $ break (== "") input
  print $ "part 1: " <> ((\(Stack s) -> head s) <$> performMovesWith stacks moves reverse)
  print $ "part 2: " <> ((\(Stack s) -> head s) <$> performMovesWith stacks moves id)
