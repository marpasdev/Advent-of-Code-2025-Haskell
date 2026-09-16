module Main
    where

import Day1
import Day2 
import Day3

import System.IO
import Data.List.Split (splitOn)

day1 :: IO ()
day1 = do
    let dial = 50
    handle <- openFile "input/rotations.txt" ReadMode
    contents <- hGetContents handle
    let rotations = parseRotation <$> (lines contents)
    print $ countDialLeftOnZero dial rotations
    hClose handle

day1part2 :: IO ()
day1part2 = do
    let dial = 50
    contents <- readFile "input/rotations.txt"
    let rotations = parseRotation <$> (lines contents)
    print $ countDialOnZero dial rotations

day2 :: IO ()
day2 = do
    contents <- readFile "input/unchecked_ranges.txt"
    let ranges = parseRange <$> (splitOn "," contents)
    let idsToCheck = concat $ getRange <$> ranges
    let invalidIDs = filter (not . isValidProductID) idsToCheck
    print $ sum invalidIDs

day2part2 :: IO ()
day2part2 = do
    contents <- readFile "input/unchecked_ranges.txt"
    let ranges = parseRange <$> (splitOn "," contents)
    let idsToCheck = concat $ getRange <$> ranges
    let invalidIDs = filter (not . isValidProductIDExtended) idsToCheck
    print $ sum invalidIDs

day3 :: IO ()
day3 = do
    contents <- readFile "input/batteries.txt"
    let banks = lines contents
    let maxJoltages = maxJoltage <$> banks
    let totalJoltage = sum maxJoltages
    print totalJoltage

main :: IO ()
main = do
    day3