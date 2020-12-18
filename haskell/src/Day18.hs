module Day18 (day18) where

import Text.Parsec

day18 :: IO ()
day18 = do
    input <- readFile "../data/day18.txt"
    let answer1 = sum $ map (parseLine parseBasic) $ lines input
    let answer2 = sum $ map (parseLine parseAdvanced) $ lines input

    putStrLn "\n===== Day 18 ====="
    putStrLn $ "The sum of the basic problem solutions is " ++ show answer1
    putStrLn $ "The sum of the advanced problem solutions is " ++ show answer2


parseLine :: Parsec String () Int -> String -> Int
parseLine parser s =
    case parse parser "" s of
        Right n -> n
        Left e -> error $ show e

-- <exp> ::= <term> { ("+" | "*") <term> }
-- <term> ::= <int> | "(" <exp> ")"
parseBasic :: Parsec String () Int
parseBasic = do
    term <- parseTerm
    loopTerm term
    where
        parseTerm = parseNum <|> parseParens parseBasic
        termTail t1 = do
            op <- spaces *> oneOf "+*"
            t2 <- spaces *> parseTerm
            spaces
            case op of
                '+' -> loopTerm (t1 + t2)
                '*' -> loopTerm (t1 * t2)
        loopTerm t = termTail t <|> return t

-- <exp> ::= <factor> { "*" <factor> }
-- <factor> ::= <term> { "+" <term> }
-- <term> ::= <int> | "(" <exp> ")"
parseAdvanced :: Parsec String () Int
parseAdvanced = do
    factor <- parseFactor
    loopFactor factor
    where
        parseFactor = do
            t <- parseTerm
            loopTerm t
        loopFactor t = factorTail t <|> return t
        factorTail t1 = do
            char '*'; spaces
            t2 <- parseFactor
            spaces
            loopFactor (t1 * t2)

        parseTerm = parseNum <|> parseParens parseAdvanced
        termTail t1 = do
            char '+'; spaces
            t2 <- parseTerm
            spaces
            loopTerm (t1 + t2)
        loopTerm t = termTail t <|> return t

parseParens :: Parsec String () Int -> Parsec String () Int
parseParens exprParser = do
    char '(' ; spaces
    result <- exprParser
    spaces ; char ')'
    spaces
    return result

parseNum :: Parsec String () Int
parseNum = do
    ds <- many1 digit
    spaces
    return $ read ds