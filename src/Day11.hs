{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Day11 (day11) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Foldable (Foldable (foldl'))
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Ord (Down (Down))
import qualified Data.Sequence as S
import qualified Text.Parsec as P

data Monkey where
  Monkey ::
    { items :: S.Seq Int,
      operation :: Int -> Int,
      test :: Int,
      testFn :: Int -> Int,
      inspectCount :: Int
    } ->
    Monkey

clear :: Monkey -> Monkey
clear m@Monkey {..} = m {items = S.empty, inspectCount = inspectCount + length items}

add :: Int -> Monkey -> Monkey
add i m@Monkey {..} = m {items = items S.|> i}

-- parse

parseMonkeys :: P.SourceName -> String -> Either P.ParseError [Monkey]
parseMonkeys = P.parse (P.sepBy1 parseMonkey P.newline <* P.eof)
  where
    parseInt :: P.Parsec String () Int
    parseInt = read <$> P.many1 P.digit

    parseItems :: P.Parsec String () [Int]
    parseItems = P.string "  Starting items: " *> P.sepBy1 parseInt (P.string ", ") <* P.newline

    parseOperation :: P.Parsec String () (Int -> Int)
    parseOperation =
      do
        P.string "  Operation: new = old "
        ( (P.string "* " *> (((*) <$> parseInt) <|> (join (*) <$ P.string "old")))
            <|> (P.string "+ " *> (((+) <$> parseInt) <|> (join (+) <$ P.string "old")))
          )
          <* P.newline

    parseTarget :: P.Parsec String () (Int, Int -> Int)
    parseTarget = do
      v <- P.string "  Test: divisible by " *> parseInt <* P.newline
      t <- P.string "    If true: throw to monkey " *> parseInt <* P.newline
      f <- P.string "    If false: throw to monkey " *> parseInt <* P.newline
      pure (v, \x -> if x `mod` v == 0 then t else f)

    parseMonkey :: P.Parsec String () Monkey
    parseMonkey = do
      P.string "Monkey " <* parseInt <* P.char ':' <* P.newline
      items <- S.fromList <$> parseItems
      operation <- parseOperation
      (test, testFn) <- parseTarget
      let inspectCount = 0
      pure Monkey {..}

-- solution

turn :: M.Map Int Monkey -> Int -> M.Map Int Monkey
turn map i =
  foldl'
    (\map' x -> M.adjust (add x) (testFn x) map')
    (M.adjust clear i map)
    (operation <$> items)
  where
    Monkey {..} = map M.! i

run :: M.Map Int Monkey -> Int -> Int
run monkeys rounds = product . take 2 . sortOn Down . (inspectCount <$>) . M.elems $ result
  where
    result :: M.Map Int Monkey
    result = foldl' turn monkeys . concat . replicate rounds . M.keys $ monkeys

day11 :: IO ()
day11 = do
  input <- readFile "puzzle-input/day11"
  case parseMonkeys "day11" input of
    Left err -> print err
    Right monkeys -> do
      let monkeysMap = M.fromList $ zip [0 ..] monkeys

      let monkeysDiv3 = M.map (\m@Monkey {..} -> m {operation = (`div` 3) . operation}) monkeysMap
      putStrLn $ "part 1: " <> show (run monkeysDiv3 20)

      let base = foldr1 lcm (test <$> monkeys)
      let monkeysModBase = M.map (\m@Monkey {..} -> m {operation = (`mod` base) . operation}) monkeysMap
      putStrLn $ "part 2: " <> show (run monkeysModBase 10000)
