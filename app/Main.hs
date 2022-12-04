module Main where

import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)

days :: [IO ()]
days = [day1, day2, day3, day4]

main :: IO ()
main = do
  putStrLn "Which day?"
  day <- readLn
  days !! (day - 1)
