{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day7 where

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T

import           Data.Char                      ( digitToInt )
import           Data.Maybe

day7 :: T.Text -> T.Text
day7 = undefined

day7part1 :: T.Text -> Int
day7part1 = undefined

day7part2 :: T.Text -> T.Text
day7part2 = undefined

-- ==================================================================
-- Data, classes, types and static values
-- ==================================================================
type Node = T.Text
type Vertex = (Node, Node, Int)
type Graph = [Vertex]

-- ==================================================================
-- Functions
-- ==================================================================