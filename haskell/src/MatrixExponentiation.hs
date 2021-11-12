module MatrixExponentiation (calc) where

type Matrix a = [[a]]

calc :: Matrix Integer -> Integer -> Matrix Integer
calc m n
  | n == 0 = identity (length m)
  | even n = let half = calc m (n `div` 2) in mul half half
  | otherwise = let prev = calc m (n - 1) in mul prev m

identity :: Int -> Matrix Integer
identity 1 = [[1]]
identity n = (1 : replicate (n-1) 0) : map (0:) (identity $ n-1)

mul :: Matrix Integer -> Matrix Integer -> Matrix Integer
mul m1 m2 = map mulRow m1
  where
    m2' = transpose m2
    mulRow r1 = map (mulRowCol r1) m2'
    mulRowCol r1 c2 = sum $ zipWith (*) r1 c2
    
transpose :: Matrix a -> Matrix a
transpose m = map (col m) [0..(length m)]

row :: Matrix a -> Int -> [a]
row = (!!)

col :: Matrix a -> Int -> [a]
col m n = map (!! n) m