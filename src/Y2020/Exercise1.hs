{-# LANGUAGE OverloadedStrings #-}
module Y2020.Exercise1 
where

import qualified Data.Text as T
import qualified Data.Map as Map

import Data.Maybe (isNothing, fromMaybe)
import Utils (openFile, printToOutput)

exercise1 :: IO ()
exercise1 = do
    contents <- openFile "resources/2020/exercise1.txt"
    printToOutput (mconcat ["Result Day 1 - first part: ", T.pack . show . findAnswer2 $ contents])
    printToOutput (mconcat ["Result Day 1 - second part: ", T.pack . show . findAnswer2 $ contents])

findAnswer2 :: T.Text -> Int
findAnswer2 numbers = fromMaybe 0 (traverseNumbers2 (T.lines numbers) Map.empty)

traverseNumbers2 :: [T.Text] -> Map.Map Int Int -> Maybe Int
traverseNumbers2 [] m = Map.lookup 2020 m
traverseNumbers2 (x:xs) m = 
    if isNothing val
    then traverseNumbers2 xs m'
    else val
    where m' = Map.insert y' (x' * y') m
          x' = read . T.unpack $ x
          y' = 2020 - x'
          val = Map.lookup x' m

