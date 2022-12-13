module Day13 (day13) where

import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)
import qualified Text.Parsec as P

data PacketData = Single Int | List [PacketData] deriving (Show, Eq)

comparePacketData :: PacketData -> PacketData -> Ordering
comparePacketData (Single a) (Single b) = compare a b
comparePacketData a@(Single _) b@(List _) = comparePacketData (List [a]) b
comparePacketData a@(List _) b@(Single _) = comparePacketData a (List [b])
comparePacketData (List []) (List []) = EQ
comparePacketData (List []) (List _) = LT
comparePacketData (List _) (List []) = GT
comparePacketData (List (a : as)) (List (b : bs)) = case comparePacketData a b of
  EQ -> comparePacketData (List as) (List bs)
  x -> x

parseInput :: P.SourceName -> String -> Either P.ParseError [(PacketData, PacketData)]
parseInput =
  P.parse $
    P.sepBy1
      ((,) <$> (parsePacketData <* P.newline) <*> (parsePacketData <* P.newline))
      P.newline
      <* P.eof
  where
    parseList :: P.Parsec String () PacketData
    parseList = List <$> P.between (P.char '[') (P.char ']') (P.sepBy parsePacketData (P.char ','))

    parseSingle :: P.Parsec String () PacketData
    parseSingle = Single . read <$> P.many1 P.digit

    parsePacketData :: P.Parsec String () PacketData
    parsePacketData = parseList P.<|> parseSingle

part1 :: [(PacketData, PacketData)] -> Int
part1 = sum . (fst <$>) . filter ((/= GT) . snd) . zip [1 ..] . (uncurry comparePacketData <$>)

part2 :: [(PacketData, PacketData)] -> Int
part2 = decode . sortBy comparePacketData . (dividers ++) . concatMap (\(a, b) -> [a, b])
  where
    dividers :: [PacketData]
    dividers = [List [List [Single 2]], List [List [Single 6]]]

    decode :: [PacketData] -> Int
    decode xs = product $ (+ 1) . fromJust . flip elemIndex xs <$> dividers

day13 :: IO ()
day13 = do
  input <- parseInput "day13" <$> readFile "puzzle-input/day13"
  case input of
    Left err -> print err
    Right packetData -> do
      putStrLn $ "part 1: " <> show (part1 packetData)
      putStrLn $ "part 2: " <> show (part2 packetData)
