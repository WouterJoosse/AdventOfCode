{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day6 where

import qualified Data.Text                     as T

import Data.List (nub)
import           Utils                          ( splitInputOnEmptyLine )

day6 :: T.Text -> T.Text
day6 input =
  let groups  = parseGroups input
      result1 = day6part1 groups
      result2 = day6part2 groups
  in  mconcat
        [ "Result Day 1 - first part: "
        , T.pack . show $ result1
        , "\n"
        , "Result Day 1 - second part: "
        , T.pack . show $ result2
        ]

day6part1 :: [Group] -> Int
day6part1 = sum . map distinctAnswers

day6part2 :: [Group] -> Int
day6part2 = undefined
-- ==================================================================
-- Data, classes, types and static values
-- ==================================================================
type Group = [T.Text]
-- ==================================================================
-- Functions
-- ==================================================================
-- | Parse the input in a list of groups
parseGroups :: T.Text -> [Group]
parseGroups = splitInputOnEmptyLine

-- | Count the number of distinct answers in a group
distinctAnswers :: Group -> Int
distinctAnswers = length . nub . concatMap distinctChars

distinctChars :: T.Text -> [T.Text]
distinctChars = nub . T.chunksOf 1