module Day13 (day13) where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromJust)

type BusNr = Int

type Time = Int

type Bus = Maybe BusNr

day13 :: IO ()
day13 = do
  s <- readFile "../data/day13.txt"
  let (time, buses) = parseInput s
  let (waitingTime, busNr) = getMinWaitTime time buses
  let answer2 = getFunnyTimestamp buses
  putStrLn "\n===== Day 13 ====="
  putStrLn $ "Bus " ++ show busNr ++ " arrives after " ++ show waitingTime ++ ". (" ++ show (waitingTime * busNr) ++ ")"
  putStrLn $ "The sequential buses starts running at " ++ show answer2

getMinWaitTime :: Time -> [Bus] -> (Time, BusNr)
getMinWaitTime time buses =
  let waitingTimeFor busNr = busNr - time `mod` busNr
      waitingTimes = map (\busNr -> (waitingTimeFor busNr, busNr)) $ catMaybes buses
   in minimum waitingTimes

getFunnyTimestamp :: [Bus] -> Int
getFunnyTimestamp buses =
  go 0 (fromJust $ head buses) $ tail $ indexBuses buses
  where
    go n _ [] = n
    go n mul ((i, busNr) : bs) =
      go
        (getNextNumberWithRemainder n mul busNr (busNr - i))
        (busNr * mul)
        bs

getNextNumberWithRemainder :: Int -> Int -> Int -> Int -> Int
getNextNumberWithRemainder start step divider remainder =
  head $ filter (\n -> n `rem` divider == remainder) [start, (start + step) ..]

indexBuses :: [Bus] -> [(Int, BusNr)]
indexBuses = go 0
  where
    go _ [] = []
    go n (Nothing : bs) = go (n + 1) bs
    go n (Just b : bs) = (n `rem` b, b) : go (n + 1) bs

parseInput :: String -> (Int, [Bus])
parseInput s =
  let [l1, l2] = lines s
      toBus "x" = Nothing
      toBus n = Just (read n)
   in (read l1, map toBus $ splitOn "," l2)
