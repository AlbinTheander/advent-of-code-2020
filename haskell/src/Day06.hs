module Day06 where

import Data.Set (Set, fromList, union, intersection)

type Answer = Char
type Group = [Set Answer]

day06 :: IO ()
day06 = do
    input <- readFile "../data/day06.txt"
    let groups = parseGroups input
    let answer1 = sum . map (length . answersByAnyone) $ groups
    let answer2 = sum . map (length . answersByEveryone) $ groups
    putStrLn "\n===== Day 6 ====="
    putStrLn $ "The sum of answers that anyone in the group affirmed is " ++ show answer1
    putStrLn $ "The sum of answers that everyone in the group affirmed is " ++ show answer2

parseGroups :: String -> [Group]
parseGroups = map (map fromList) . parseGroups' [] . lines
    where
        parseGroups' people [] = [people]
        parseGroups' people ("" : rest) = people : parseGroups' [] rest
        parseGroups' people (p : rest) = parseGroups' (p: people) rest

answersByAnyone :: Group -> Set Answer
answersByAnyone = foldl1 union

answersByEveryone :: Group -> Set Answer
answersByEveryone = foldl1 intersection