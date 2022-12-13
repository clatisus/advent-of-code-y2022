{-# LANGUAGE InstanceSigs #-}

module Day13 (day13) where

import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import qualified Text.Parsec as P

data Packet = S Int | L [Packet] deriving (Show, Eq)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (S a) (S b) = compare a b
  compare a@(S _) b@(L _) = compare (L [a]) b
  compare a@(L _) b@(S _) = compare a (L [b])
  compare (L []) (L []) = EQ
  compare (L []) (L _) = LT
  compare (L _) (L []) = GT
  compare (L (a : as)) (L (b : bs)) = compare a b <> compare (L as) (L bs)

parseInput :: P.SourceName -> String -> Either P.ParseError [(Packet, Packet)]
parseInput =
  P.parse $
    ((,) <$> (parse <* P.newline) <*> (parse <* P.newline))
      `P.sepBy1` P.newline
      <* P.eof
  where
    parse :: P.Parsec String () Packet
    parse =
      S . read <$> P.many1 P.digit
        P.<|> L <$> P.between (P.char '[') (P.char ']') (parse `P.sepBy` P.char ',')

part1 :: [(Packet, Packet)] -> Int
part1 = sum . (fst <$>) . filter ((/= GT) . snd) . zip [1 ..] . (uncurry compare <$>)

part2 :: [(Packet, Packet)] -> Int
part2 = decode . sort . (dividers ++) . concatMap (\(a, b) -> [a, b])
  where
    dividers :: [Packet]
    dividers = [L [L [S 2]], L [L [S 6]]]

    decode :: [Packet] -> Int
    decode xs = product $ (+ 1) . fromJust . flip elemIndex xs <$> dividers

day13 :: IO ()
day13 = do
  input <- parseInput "day13" <$> readFile "puzzle-input/day13"
  case input of
    Left err -> print err
    Right packetData -> do
      putStrLn $ "part 1: " <> show (part1 packetData)
      putStrLn $ "part 2: " <> show (part2 packetData)
