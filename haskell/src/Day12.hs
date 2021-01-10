module Day12 (day12) where

import Vector2

data Instruction = Instruction {op :: Char, arg :: Int} deriving (Show)
type Position = V2
type Direction = V2

data Ship = Ship Position Direction
          | WpShip Position Position



day12 :: IO ()
day12 = do
  s <- readFile "../data/day12.txt"
  let instructions = parseInstructions s
  let Ship (x1, y1) _ = foldl execute (Ship zero east) instructions 
  let WpShip (x2, y2) _ = foldl execute (WpShip zero (10, 1)) instructions
  putStrLn "\n===== Day 12 ====="
  putStrLn $ "The first ship would end up at " ++ show (x1, y1) ++ " (" ++ show (abs x1 + abs y1) ++ ")."
  putStrLn $ "The correct ship would end up at " ++ show (x2, y2) ++ " (" ++ show (abs x2 + abs y2) ++ ")."

parseInstructions :: String -> [Instruction]
parseInstructions =
  map parseInstr . lines
  where
    parseInstr line = Instruction (head line) (read $ tail line)

execute :: Ship -> Instruction -> Ship
execute (Ship pos direction) instr =
    case instr of
        Instruction 'N' n -> Ship (pos $+ north $* n) direction
        Instruction 'E' n -> Ship (pos $+ east $* n) direction
        Instruction 'S' n -> Ship (pos $+ south $* n) direction
        Instruction 'W' n -> Ship (pos $+ west $* n) direction
        Instruction 'L' n -> Ship pos (turnLeft direction n)
        Instruction 'R' n -> Ship pos (turnRight direction n)
        Instruction 'F' n -> Ship (pos $+ direction $* n) direction
execute (WpShip pos waypoint) instr =
    case instr of
        Instruction 'N' n -> WpShip pos (waypoint $+ north $* n)
        Instruction 'E' n -> WpShip pos (waypoint $+ east $* n)
        Instruction 'S' n -> WpShip pos (waypoint $+ south $* n)
        Instruction 'W' n -> WpShip pos (waypoint $+ west $* n)
        Instruction 'L' n -> WpShip pos (turnLeft waypoint n)
        Instruction 'R' n -> WpShip pos (turnRight waypoint n)
        Instruction 'F' n -> WpShip (pos $+ waypoint $* n) waypoint
