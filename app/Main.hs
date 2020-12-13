{-# LANGUAGE OverloadedStrings #-}
module Main where

import Utils (openFile, printToOutput)

import Y2020.Day1 (day1)
import Y2020.Day2 (day2)
import Y2020.Day3 (day3)
import Y2020.Day4 (day4)
import Y2020.Day5 (day5)

main :: IO ()
main = do
    input <- openFile "resources/2020/day1.txt"
    printToOutput . day1 $ input
    input <- openFile "resources/2020/day2.txt"
    printToOutput . day2 $ input
    input <- openFile "resources/2020/day3.txt"
    printToOutput . day3 $ input
    input <- openFile "resources/2020/day4.txt"
    printToOutput . day4 $ input
    input <- openFile "resources/2020/day5.txt"
    printToOutput . day5 $ input
