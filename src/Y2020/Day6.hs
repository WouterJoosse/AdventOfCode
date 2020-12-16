{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day6 where

import qualified Data.Text                     as T
import qualified Data.Map                      as Map

import           Data.List                      ( nub )
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
day6part1 = sum . map unionAnswers

day6part2 :: [Group] -> Int
day6part2 = sum . map intersectAnswers

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

-- | Count the union of the answers in a group
unionAnswers :: Group -> Int
unionAnswers = length . nub . concatMap unionChars

-- | Create a set of all distinct characters in the text
unionChars :: T.Text -> [T.Text]
unionChars = nub . T.chunksOf 1

-- | Count the number of answers that occur in all answers of the group
intersectAnswers :: Group -> Int
intersectAnswers g = length . Map.filter (== length g) $ intersectionMap
-- length g is the number of persons in a group, so get the length of the list of characters that occur in all answers of all persons
  where intersectionMap = intersectAnswers' g

-- | Create a map of the count of each character in all persons in a group
intersectAnswers' :: Group -> Map.Map T.Text Int
intersectAnswers' []       = Map.empty
intersectAnswers' (x : xs) = Map.unionWith (+) personMap (intersectAnswers' xs)
 where
  personMap = foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty (unionChars x)
