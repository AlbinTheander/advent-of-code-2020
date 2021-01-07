module Day07 (day07) where

import Data.Map (Map, empty, fromList, member, toList, foldrWithKey)
import Data.List (nub)
import Debug.Trace

data Bag = Bag { color :: String, contents :: Map String Int }
    deriving (Show, Eq)

day07 :: IO ()
day07 = do 
    s <- readFile "../data/day07.txt"
    -- s <- readFile "d07.txt"
    let bags = map parseBagDefinition (lines s)
    let answer1 = length $ nub $ containsBag bags "shiny gold"
    let answer2 = containedBags bags "shiny gold"
    putStrLn "\n===== Day 7 ====="
    putStrLn $ "The number of bags that can contain a shiny gold bag is " ++ show answer1
    putStrLn $ "One shiny gold bag contains " ++ show answer2 ++ " other bags."

containsBag :: [Bag] -> String -> [Bag]
containsBag []   _     = []
containsBag bags bagColor = 
    let containers = filter (member bagColor . contents) bags
        containerContainers = concatMap (containsBag bags . color) containers
    in
        containers ++ containerContainers

containedBags :: [Bag] -> String -> Int
containedBags bags col =
    let bag = head $ filter ((==) col . color) bags
    in
        foldrWithKey (\col n acc -> acc + n * (1 + containedBags bags col)) 0 $ contents bag

parseBagDefinition :: String -> Bag
parseBagDefinition s = 
    let ws = words s
        colorAt x = (ws !! x) ++ " " ++ (ws !! (x+1))
        countedBagAt x = (colorAt (x+1), read (ws !! x))
    in
        case length ws of
            -- pale yellow bags contain no other bags.
            7 -> Bag (colorAt 0) empty 
            -- faded blue bags contain 3 dark gree bags.
            _ -> Bag (colorAt 0) (fromList [countedBagAt i | i <- [4, 8 .. length ws - 1]])
