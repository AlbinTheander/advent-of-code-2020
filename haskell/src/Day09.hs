module Day09 (day09) where

import Data.Vector (Vector, (!), fromList)
import Data.Maybe (mapMaybe)

day09 :: IO ()
day09 = do
    s <- readFile "../data/day09.txt"
    let ns = parseNumbers s
    let answer1 = firstWeakness ns 25
    let answer2 = part2 ns answer1
    putStrLn "\n===== Day 9 ====="
    putStrLn $ "The first number exposing the weakness is " ++ show answer1
    putStrLn $ "The checksum for the sequence adding up to the weak number is " ++ show answer2

firstWeakness :: Vector Int -> Int -> Int
firstWeakness ns preambleSize =
    (!) ns $ head $ filter (isWeakness ns) [preambleSize .. (length ns - 1)]

isWeakness :: Vector Int -> Int -> Bool
isWeakness ns pos = 
    let
        indexPairs = [(i, j) | i <- [(pos-25) .. (pos-2)],
                               j <- [(i+1) .. (pos-1)] ]
        pairSums = map (\(i, j) -> (ns ! i) + (ns ! j)) indexPairs
    in
        (ns ! pos) `notElem` pairSums

part2 :: Vector Int -> Int -> Int
part2 ns target =
    let
        seq = findSequenceWithSum ns target
    in
        minimum seq + maximum seq

findSequenceWithSum :: Vector Int -> Int -> [Int]
findSequenceWithSum ns target =
    head $ mapMaybe (sequenceToTarget ns target) [0 .. (length ns - 1)]


sequenceToTarget :: Vector Int -> Int -> Int -> Maybe [Int]
sequenceToTarget ns target start =
    go start [] target
    where
        go _ result 0 = Just result
        go i result target = if target < 0 
                             then Nothing 
                             else go (i+1) ((ns ! i) : result) (target - (ns ! i))

parseNumbers :: String -> Vector Int
parseNumbers = fromList . map read . lines