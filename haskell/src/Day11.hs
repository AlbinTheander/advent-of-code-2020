module Day11 (day11) where

import Data.Map (Map, elems, fromList, lookup, mapWithKey, (!))
import Data.Maybe (mapMaybe)
import Prelude hiding (lookup)

type Coord = (Int, Int)

type Room = Map Coord Char
type EvolveFn = Room -> Coord -> Char -> Char

(<+>) :: Coord -> Coord -> Coord
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

day11 :: IO ()
day11 = do
  s <- readFile "../data/day11.txt"
  let room = parseRoom s
  let room1 = simulate room evolveSpot1
  let room2 = simulate room evolveSpot2
  let answer1 = length $ filter ('#' ==) $ elems room1
  let answer2 = length $ filter ('#' ==) $ elems room2
  putStrLn "\n===== Day 11 ====="
  putStrLn $ "The original estimate of occupied seats was " ++ show answer1
  putStrLn $ "The correct number of occupied seats is " ++ show answer2


parseRoom :: String -> Room
parseRoom s =
  let indexedLines = zip [0 ..] (lines s)
      indexedSpots = concatMap (\(y, line) -> zipWith (\x ch -> ((x, y), ch)) [0 ..] line) indexedLines
   in fromList indexedSpots

neighbours = [(i, j) | i <- [-1, 0, 1], j <- [-1, 0, 1], j /= 0 || i /= 0]

simulate :: Room -> EvolveFn -> Room
simulate room evolveSpot = 
  let evolve r = mapWithKey (evolveSpot r) r
   in untilStable evolve room

evolveSpot1 :: EvolveFn
evolveSpot1 room pos current =
  let neighbourCount = length $ filter (isCloseNeighbourOccupied room pos) neighbours
   in case current of
        'L' | neighbourCount == 0 -> '#'
        '#' | neighbourCount >= 4 -> 'L'
        _ -> current

isCloseNeighbourOccupied :: Room -> Coord -> Coord -> Bool
isCloseNeighbourOccupied room pos direction =
  (pos <+> direction) `lookup` room == Just '#'

evolveSpot2 :: Room -> Coord -> Char -> Char
evolveSpot2 room pos ch =
  let neighbourCount = length $ filter (isFarNeighbourOccupied room pos) neighbours
   in case ch of
        'L' | neighbourCount == 0 -> '#'
        '#' | neighbourCount >= 5 -> 'L'
        _ -> ch

isFarNeighbourOccupied :: Room -> Coord -> Coord -> Bool
isFarNeighbourOccupied room pos direction =
  go (pos <+> direction)
  where
    go p = case lookup p room of
      Nothing -> False
      Just '#' -> True
      Just 'L' -> False
      Just '.' -> go (p <+> direction)

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f seed =
  let next = f seed
   in if next == seed then seed else untilStable f next