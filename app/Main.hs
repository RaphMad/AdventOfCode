module Main where

import ParseInput

main :: IO ()
main = do
    input <- parseDay1
    print $ day1 input

day1 :: [Integer] -> Integer
day1 = sum
