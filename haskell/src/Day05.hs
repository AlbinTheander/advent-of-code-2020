module Day05 where

import Data.List (sort)

day05 :: IO ()
day05 = do
    input <- readFile "../data/day05.txt"
    let seats = parseInput input
    let maxSeat = maximum seats
    let mySeat = findMySeat seats
    putStrLn "\n===== Day 5 ====="
    putStrLn $ "The highest seat number is " ++ show maxSeat
    putStrLn $ "My seat number is " ++ show mySeat

parseInput :: String -> [Int]
parseInput = map (seatNr 0) . lines
    where
        seatNr n [] = n
        seatNr n (ch : chs)
            | (ch == 'F') || (ch == 'L') = seatNr (2*n) chs
            | otherwise = seatNr (2*n + 1) chs

findMySeat :: [Int] -> Int
findMySeat = findMySeat' . sort
    where
        findMySeat' (n1:n2:ns) = 
            if n2 == (n1 + 2) 
                then n1 + 1 
                else findMySeat' (n2:ns)