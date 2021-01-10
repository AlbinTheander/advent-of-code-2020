module Vector2 (V2, ($+), ($-), ($*), north, east, south, west, zero, turnLeft, turnRight) where

type V2 = (Int, Int)

infixl 5 $+
($+) :: V2 -> V2 -> V2
(x1, y1) $+ (x2, y2) = (x1+x2, y1+y2)

infixl 5 $-
($-) :: V2 -> V2 -> V2
(x1, y1) $- (x2, y2) = (x1-x2, y1-y2)

infixl 7 $*
($*) :: V2 -> Int -> V2
(x, y) $* n = (x * n, y * n)

turnLeft :: V2 -> Int -> V2
turnLeft v 0 = v
turnLeft (x, y) degrees = turnLeft (-y, x) (degrees - 90)

turnRight :: V2 -> Int -> V2
turnRight v degrees = turnLeft v (360 - degrees)

zero = (0, 0) :: V2
north = (0, 1) :: V2
east = (1, 0) :: V2
south = (0, -1) :: V2
west = (-1, 0) :: V2
