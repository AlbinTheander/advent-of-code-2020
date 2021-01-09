module Day10 (day10) where

import Data.List (sort)

day10 :: IO ()
day10 = do
    s <- readFile "../data/day10.txt"
    let adapters = sort (map read $ lines s) :: [Int]
    let (ones, threes) = countJumps adapters
    let answer1 = ones * threes
    let answer2 = countArrangements adapters
    putStrLn "\n===== Day 10 ====="
    putStrLn $ "The number of 1 and 3-jumps are " ++ show (ones, threes) ++ " and their product is " ++ show answer1
    putStrLn $ "The adapters can be arranged in " ++ show answer2 ++ " ways."
    


countJumps :: [Int] -> (Int, Int)
countJumps = go 0 0 0 where
    go current ones threes [] = (ones, threes + 1)
    go current ones threes (n:ns) = case n - current of
        1 -> go n (ones+1) threes ns
        3 -> go n ones (threes+1) ns
        _ -> go n ones threes ns

countArrangements :: [Int] -> Int
countArrangements ns = let
    groups = contiguousGroups ns
    groupSizes = map length groups
    groupArrangements = map tribonacci groupSizes
    in
        product groupArrangements

-- This is the tribonacci sequence starting with 0, 1, 1.
-- This is also the number of ways to arrange n adapters with contiguous joltages
tribonacci :: Int -> Int
tribonacci 0 = 0
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci n = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)

contiguousGroups :: [Int] -> [[Int]]
contiguousGroups = go 0 [0] [] where
    go current currentGroup groups [] = currentGroup : groups
    go current currentGroup groups (n:ns) = case n - current of
        1 -> go n (n:currentGroup) groups ns
        _ -> go n [n] (currentGroup:groups) ns