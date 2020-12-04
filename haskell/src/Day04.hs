module Day04 (day04) where

import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

type Passport = [Field]
type Field = (String, String)

day04 :: IO ()
day04 = do
    s <- readFile "../data/day04.txt"
    let passports = parsePassports $ lines s
    let complete = filter isComplete passports
    let valid = filter isValid complete
    putStrLn "\n===== Day 5 ====="
    putStrLn $ "There are " ++ show (length complete) ++ " complete passports"
    putStrLn $ "There are " ++ show (length valid) ++ " valid passports"

parsePassports :: [String] -> [Passport]
parsePassports = map createPassport . parsePassports' ""
    where
        parsePassports' current [] = [current]
        parsePassports' current ("" : rest) = current : parsePassports' "" rest
        parsePassports' current (s : rest) = parsePassports' (current ++ " " ++ s) rest

createPassport :: String -> Passport
createPassport = map makeField . words
    where
        listToTuple [a, b] = (a, b)
        makeField s = listToTuple $ splitOn ":" s

mandatoryFields :: [String]
mandatoryFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isComplete :: Passport -> Bool
isComplete passport = all (\field -> isJust $ lookup field passport) mandatoryFields

isValid :: Passport -> Bool
isValid = all isValidField

isValidField :: Field -> Bool
isValidField ("byr", value) = validYear value 1920 2002
isValidField ("iyr", value) = validYear value 2010 2020
isValidField ("eyr", value) = validYear value 2020 2030
isValidField ("hgt", value) = validHeight value
isValidField ("hcl", value) = value =~ "^#[0-9a-f]{6}"
isValidField ("ecl", value) = value =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"
isValidField ("pid", value) = value =~ "^[0-9]{9}$"
isValidField _ = True


validYear :: String -> Int -> Int -> Bool
validYear s from to =  (length s == 4) && maybeBetween (readMaybe s) from to

validHeight :: String -> Bool
validHeight value
    | value =~ "cm$" = maybeBetween (readMaybe . init . init $ value) 150 193 
    | value =~ "in$" = maybeBetween (readMaybe . init . init $ value) 59 76 
    | otherwise = False
    where

maybeBetween :: Maybe Int -> Int -> Int -> Bool
maybeBetween  (Just n) from to = from <= n && n <= to
maybeBetween Nothing _ _  = False