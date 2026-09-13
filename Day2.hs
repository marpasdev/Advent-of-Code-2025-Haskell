module Day2
    where

import Data.Char (isDigit)
import Data.List (intersect)

parseRange :: String -> (Int, Int)
parseRange xs = (rangeStart, rangeEnd)
    where
        rangeStart = read (takeWhile isDigit xs) :: Int
        rangeEnd = read (takeWhile isDigit rest) :: Int
            where
                rest = tail $ dropWhile isDigit xs

getRange :: (Int, Int) -> [Int]
getRange (start, end) = [start .. end]

type ID = Int

isValidProductID :: ID -> Bool
isValidProductID id = let
    firstHalf = take half ids
    secondHalf = drop half ids
        in firstHalf /= secondHalf
            where
                ids = show id
                half = length ids `div` 2

-- Part Two
groupList :: [a] -> Int -> [[a]]
groupList [] _ = []
groupList xs n = if length xs `mod` n /= 0 
    then error "Grouping error."
    else take n xs : groupList (drop n xs) n

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

isValidProductIDExtended :: ID -> Bool
isValidProductIDExtended id = aux id 1
    where
        aux id i
            | i > half = True
            | length ids `mod` i /= 0 = aux id (i + 1)
            | allEqual (groupList ids i) = False
            | otherwise = aux id (i + 1)
            where
                ids = show id
                half = length ids `div` 2