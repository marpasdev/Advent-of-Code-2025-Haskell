module Main
    where

import Day1
import System.IO

main :: IO ()
main = do
    let dial = 50
    handle <- openFile "rotations.txt" ReadMode
    contents <- hGetContents handle
    let rotations = parseRotation <$> (lines contents)
    print $ countDialOnZero dial rotations
    hClose handle