module Day08 (day08) where

import Data.Vector (Vector, (!), (!?), (//), fromList)

data Instruction = Acc Int | Jmp Int | Nop Int | Exit deriving (Show)
type Program = Vector Instruction

data Cpu = Cpu { ip :: Int, acc :: Int, ram :: Program }

day08 :: IO ()
day08 = do
    s <- readFile "../data/day08.txt"
    let program = parseProgram s
    let result1 = acc $ runUntilRepeat (Cpu 0 0 program)
    let result2 = acc $ correctAndRun program
    putStrLn "\n===== Day 8 ====="
    putStrLn $ "After running the program, the accumulator contains " ++ show result1
    putStrLn $ "After running the corrected program, it contains " ++ show result2

runUntilRepeat :: Cpu -> Cpu
runUntilRepeat cpu@Cpu{ip = ip, acc = acc, ram = ram} =
    case ram !? ip of
        Just (Acc n) -> runUntilRepeat $ Cpu (ip + 1) (acc + n) (ram // [(ip, Exit)])
        Just (Jmp n) -> runUntilRepeat $ Cpu (ip + n) acc (ram // [(ip, Exit)])
        Just (Nop _) -> runUntilRepeat $ Cpu (ip + 1) acc (ram // [(ip, Exit)])
        Just Exit    -> cpu
        Nothing      -> cpu

correctAndRun :: Program -> Cpu
correctAndRun program = 
    let
        modifiedPrograms = map (modifyProgram program) [0 .. (length program - 1)]
        results = map (runUntilRepeat . Cpu 0 0) modifiedPrograms
    in
        head $ filter (\cpu -> ip cpu == length (ram cpu)) results

modifyProgram :: Program -> Int -> Program
modifyProgram program line = 
    case program ! line of
        Nop n -> program // [(line, Jmp n)]
        Jmp n -> program // [(line, Nop n)]
        _     -> program

parseProgram :: String -> Program
parseProgram = fromList . map parseInstruction . lines

parseInstruction :: String -> Instruction
parseInstruction s = case words s of
    ["acc", n] -> Acc (readSigned n)
    ["jmp", n] -> Jmp (readSigned n)
    ["nop", n] -> Nop (readSigned n)
    where
        readSigned s = if head s == '-' then read s else read (tail s)