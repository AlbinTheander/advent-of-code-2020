module Day02 (day02) where

import qualified Text.Parsec as Parsec

data Entry = Entry Int Int Char String
instance Show Entry where
    show (Entry n1 n2 ch pw) = "Entry: " ++ show n1 ++ ", " ++ show n2 ++ " (" ++ [ch] ++ "): " ++ pw

day02 :: IO ()
day02 = do
    s <- readFile "../data/day02.txt"
    let entries = parseData s
    let answer1 = length $ filter valid1 entries
    let answer2 = length $ filter valid2 entries
    putStrLn "\n===== Day 2 ====="
    putStrLn $ "The number of valid passwords with the old policy is " ++ show answer1
    putStrLn $ "The number of valid passwords with the new policy is " ++ show answer2

valid1 :: Entry -> Bool
valid1 (Entry n1 n2 ch pw) =
    matchingChars >= n1 && matchingChars <= n2
    where
        matchingChars = length $ filter (ch ==) pw 

valid2 :: Entry -> Bool
valid2 (Entry n1 n2 ch pw) =
    (ch == pw!!(n1-1)) /= (ch == pw!!(n2-1))

parseData :: String -> [Entry]
parseData =
    map parseLine . lines

parseLine :: String -> Entry
parseLine s =
    case Parsec.parse entryParse "" s of
        Right entry -> entry
        Left _ -> Entry 0 0 ' ' "Nothing"

entryParse :: Parsec.Parsec String () Entry
entryParse = do
    n1 <- parseInt
    Parsec.char '-'
    n2 <- parseInt
    Parsec.spaces
    ch <- Parsec.anyChar
    Parsec.string ": "
    pw <- Parsec.many1 Parsec.anyChar
    return $ Entry n1 n2 ch pw

parseInt :: Parsec.Parsec String () Int
parseInt = do
    s <- Parsec.many1 Parsec.digit
    return (read s :: Int)