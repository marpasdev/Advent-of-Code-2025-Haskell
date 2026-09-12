module Day2
    where

import Data.Char (isDigit)

parseRange :: String -> (Int, Int)
parseRange xs = (rangeStart, rangeEnd)
    where
        rangeStart = read (takeWhile (isDigit) xs) :: Int
        rangeEnd = read (takeWhile (isDigit) rest) :: Int
            where
                rest = tail $ dropWhile (isDigit) xs

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