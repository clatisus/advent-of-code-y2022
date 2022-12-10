module Day10 (day10) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (void)
import Data.List (scanl')
import Data.List.Split (chunksOf)
import qualified Text.Parsec as P

data Instruction = Noop | Addx Int deriving (Show)

parseInput :: P.SourceName -> String -> Either P.ParseError [Instruction]
parseInput = P.parse (P.many1 (noop <|> addx))
  where
    num :: P.Parsec String () Int
    num = read <$> P.many1 P.digit

    value :: P.Parsec String () Int
    value = P.try (negate <$> (P.char '-' *> num)) <|> num

    newlineOrEof :: P.Parsec String () ()
    newlineOrEof = void P.newline <|> P.eof

    noop :: P.Parsec String () Instruction
    noop = Noop <$ P.string "noop" <* newlineOrEof

    addx :: P.Parsec String () Instruction
    addx = Addx <$> (P.string "addx " *> value <* newlineOrEof)

run :: [Instruction] -> [Int]
run = concatMap fst . scanl' exec ([], 1)
  where
    exec :: ([Int], Int) -> Instruction -> ([Int], Int)
    exec (_, x) Noop = ([x], x)
    exec (_, x) (Addx y) = ([x, x], x + y)

part1 :: [Instruction] -> Int
part1 = sum . zipEvery40 20 . drop 19 . run
  where
    zipEvery40 :: Int -> [Int] -> [Int]
    zipEvery40 _ [] = []
    zipEvery40 v (x : xs) = v * x : zipEvery40 (v + 40) (drop 39 xs)

part2 :: [Instruction] -> String
part2 = unlines . (zipWith draw [0 ..] <$>) . chunksOf 40 . run
  where
    draw :: Int -> Int -> Char
    draw crt sprite
      | abs (crt - sprite) <= 1 = '#'
      | otherwise = '.'

day10 :: IO ()
day10 = do
  input <- readFile "puzzle-input/day10"
  case parseInput "day10" input of
    Left err -> print err
    Right instructions -> do
      putStrLn $ "part 1: " <> show (part1 instructions)
      putStrLn $ "part 2:\n" <> part2 instructions
