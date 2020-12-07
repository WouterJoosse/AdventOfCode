{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day1 where

import qualified Data.Text                     as T
import qualified Data.Map                      as Map

import           Data.Maybe                     ( isNothing
                                                , fromMaybe
                                                )
import           Utils                          ( openFile
                                                , printToOutput
                                                )

day1 :: IO ()
day1 = do
  contents <- openFile "resources/2020/day1.txt"
  printToOutput
    (mconcat
      [ "Result Day 1 - first part: "
      , T.pack . show . findAnswer2 2020 $ contents
      ]
    )
  printToOutput
    (mconcat
      [ "Result Day 1 - second part: "
      , T.pack . show . findAnswer3 2020 $ contents
      ]
    )

-- | Find the answer to the first assignment: find two numbers that add up to a target number
findAnswer2 :: Int -> T.Text -> Int
findAnswer2 target numbers =
  fromMaybe 0 (traverseNumbers2 target (T.lines numbers) Map.empty)

-- | Find the answer to the second assignment: find three numbers that add up to a target number
findAnswer3 :: Int -> T.Text -> Int
findAnswer3 target numbers =
  fromMaybe 0 (traverseNumbers3 target (T.lines numbers))

-- | Traverse over a list of numbers, stored as text and try to find a combination of 2 numbers
-- that add up to the target. Then return the product of these two numbers
traverseNumbers2 :: Int -> [T.Text] -> Map.Map Int Int -> Maybe Int
traverseNumbers2 target []       m = Map.lookup target m
traverseNumbers2 target (x : xs) m = if isNothing val
  then traverseNumbers2 target xs m'
  else val
 where
  m'  = Map.insert y' (x' * y') m
  x'  = read . T.unpack $ x
  y'  = target - x'
  val = Map.lookup x' m

-- | Traverse over a list of numbers stored as text and try to find a combination of 3 numbers
-- that add up to the target. Then return the product of these three numbers.
traverseNumbers3 :: Int -> [T.Text] -> Maybe Int
traverseNumbers3 _      []       = Nothing
traverseNumbers3 target (x : xs) = if isNothing val
  then traverseNumbers3 target xs
  else (x' *) <$> val
 where
  val = traverseNumbers2 (target - x') xs Map.empty
  x'  = read . T.unpack $ x
