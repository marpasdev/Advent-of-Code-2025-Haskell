module Main
    where

import Day1
import System.IO

day1 :: IO ()
day1 = do
    let dial = 50
    handle <- openFile "rotations.txt" ReadMode
    contents <- hGetContents handle
    let rotations = parseRotation <$> (lines contents)
    print $ countDialLeftOnZero dial rotations
    hClose handle

day1part2 :: IO ()
day1part2 = do
    let dial = 50
    contents <- readFile "rotations.txt"
    let rotations = parseRotation <$> (lines contents)
    print $ countDialOnZero dial rotations

main :: IO ()
main = do
    day1part2