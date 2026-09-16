module Day3
    where

import Data.List (delete)

type BatteryBank = String

maxPositions :: (Ord a) => [a] -> [Int]
maxPositions [] = []
maxPositions xs = aux xs [] (maximum xs) 0
    where
        aux [] ys _ _ = ys
        aux (x:xs) ys m i
            | x == m = aux xs (i:ys) m (i + 1)
            | otherwise = aux xs ys m (i + 1)

greatestRight:: (Ord a) => [a] -> [Int] -> a
greatestRight xs ys = aux xs ys (minimum xs) 0
    where
        aux _ [] m _ = m
        aux xs ys m i
            | i `notElem` ys = aux xs ys m (i + 1)
            | newMaximum > m = aux xs (delete i ys) newMaximum (i + 1)
            | newMaximum <= m = aux xs (delete i ys) m (i + 1)
            where 
                newMaximum = maximum (drop (i + 1) xs)

maxJoltage :: BatteryBank -> Int
maxJoltage xs = 10 * greatestFirst + greatestSecond
    where
        greatestFirst = read [(maximum $ init xs)] :: Int
        greatestFirstPositions = maxPositions $ init xs
        greatestSecond = read [(greatestRight xs greatestFirstPositions)] :: Int