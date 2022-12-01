module Main where

import Day1 (day1)

days :: [IO ()]
days = [day1]

main :: IO ()
main = do
  putStrLn "Which day?"
  day <- readLn
  days !! (day - 1)
