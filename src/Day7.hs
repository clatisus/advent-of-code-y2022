module Day7 (day7) where

import Control.Arrow ((***))
import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit)
import Data.List (foldl', sort)
import Text.ParserCombinators.ReadP (ReadP, char, eof, many, many1, munch1, readP_to_S, satisfy, string, (<++))

-- http://learnyouahaskell.com/zippers

type Name = String

type Size = Int

data FSItem = File Size Name | Folder Name [FSItem] deriving (Show)

--          parent folder | items before | items after
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

-- parser

data Command = GoRoot | GoUp | GoDown Name | List [FSItem] deriving (Show)

pEndOfLine :: ReadP ()
pEndOfLine = void (char '\n') <++ eof

pName :: ReadP Name
pName = many1 $ satisfy (\c -> isAlphaNum c || c == '.')

pSize :: ReadP Size
pSize = read <$> munch1 isDigit

pGoRoot :: ReadP Command
pGoRoot = GoRoot <$ string "$ cd /" <* pEndOfLine

pGoUp :: ReadP Command
pGoUp = GoUp <$ string "$ cd .." <* pEndOfLine

pGoDown :: ReadP Command
pGoDown = GoDown <$> (string "$ cd " *> pName) <* pEndOfLine

pList :: ReadP Command
pList = List <$ string "$ ls" <* pEndOfLine <*> many (pDir <++ pFile)
  where
    pDir :: ReadP FSItem
    pDir = Folder <$> (string "dir " *> pName) <*> pure [] <* pEndOfLine

    pFile :: ReadP FSItem
    pFile = File <$> pSize <*> (char ' ' *> pName) <* pEndOfLine

parse :: String -> [Command]
parse = fst . head . filter (null . snd) . readP_to_S pCommands
  where
    pCommands :: ReadP [Command]
    pCommands = many1 $ pGoRoot <++ pGoUp <++ pGoDown <++ pList

-- execution

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsRoot :: FSZipper -> FSZipper
fsRoot (item, []) = (item, [])
fsRoot x = fsRoot . fsUp $ x

nameOf :: FSItem -> Name
nameOf (File _ name) = name
nameOf (Folder name _) = name

exec :: FSZipper -> Command -> FSZipper
exec t GoRoot = fsRoot t
exec t GoUp = fsUp t
exec t (GoDown name) = (dir, FSCrumb parent ls rs : bs)
  where
    (Folder parent items, bs) = t
    (ls, dir : rs) = break ((== name) . nameOf) items
exec t (List items) = (Folder name items, bs)
  where
    (Folder name _, bs) = t

-- solution

--             (answer, sub-folder sum)
part1' :: FSItem -> (Int, Int)
part1' (File size _) = (0, size)
part1' (Folder _ items) = if b <= 100000 then (a + b, b) else (a, b)
  where
    (a, b) = foldr ((\(a, b) -> (+ a) *** (+ b)) . part1') (0, 0) items

part1 :: FSItem -> Int
part1 = fst . part1'

part2' :: FSItem -> ([Int], Int)
part2' (File size _) = ([], size)
part2' (Folder _ items) = (b : a, b)
  where
    (a, b) = foldr ((\(a, b) -> (++ a) *** (+ b)) . part2') ([], 0) items

part2 :: FSItem -> Int
part2 fs = head . filter ((>= 30000000) . (70000000 -) . (rt -)) . sort $ a
  where
    (rt : a, _) = part2' fs

day7 :: IO ()
day7 = do
  commands <- parse <$> readFile "puzzle-input/day7"
  let fs = fst . fsRoot $ foldl' exec (Folder "/" [], []) commands
  print $ "part 1: " <> (show . part1) fs
  print $ "part 2: " <> (show . part2) fs
