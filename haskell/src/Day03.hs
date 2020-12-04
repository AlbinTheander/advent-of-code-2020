module Day03 (day03) where

day03 :: IO ()
day03 = do
    s <- readFile "../data/day03.txt"
    let terrain = parseData s
    let answer1 = countTrees terrain (3, 1)
    let answer2 = countSeveralSlopes terrain [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    putStrLn "\n===== Day 1 ====="
    putStrLn $ "The product of the first pair is " ++ show answer1
    putStrLn $ "The product of the second triplet is " ++ show answer2



parseData:: String -> [String]
parseData = lines

countSeveralSlopes :: [String] -> [(Int, Int)] -> Int
countSeveralSlopes terrain slopes =
    product $ map (countTrees terrain) slopes

countTrees :: [String] -> (Int, Int) -> Int
countTrees terrain (dx, dy) = countTrees' (0, 0)
    where
        width = length $ head terrain
        getSpot x y = (terrain !! y) !! (x `mod` width)
        countTrees' (x, y)
            | y >= length terrain = 0
            | getSpot x y == '#' = 1 + countTrees' (x + dx, y + dy)
            | otherwise = countTrees' (x + dx, y + dy)