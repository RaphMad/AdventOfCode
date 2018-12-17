module Main where

import Day1

main :: IO ()
main = do
    input <- parseDay1
    print $ day1_2 input
