module Day01 ( day01) where

import Data.List(find)
import Data.Maybe(fromJust)

day01 :: IO ()
day01 = do
    s <- readFile "../data/day01.txt"
    let nums = parseFile s
    let answer1 = part1 nums
    let answer2 = part2 nums
    putStrLn $ "The product of the first pair is " ++ show answer1
    putStrLn $ "The product of the second triplet is " ++ show answer2



parseFile:: String -> [Integer]
parseFile s = map read $ lines s

part1 :: [Integer] -> Integer
part1 nums = 
    uncurry (*) $ fromJust $ find addsTo2020 pairs
    where
        pairs = [(x, y) | x <- nums, y <- nums]
        addsTo2020 (x, y) = x + y == 2020

part2 :: [Integer] -> Integer
part2 nums = 
    mul $ fromJust $ find addsTo2020 pairs
    where
        pairs = [(x, y, z) | x <- nums, y <- nums, z <- nums]
        addsTo2020 (x, y, z) = x + y + z == 2020
        mul (a, b, c) = a * b * c

