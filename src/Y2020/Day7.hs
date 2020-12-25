{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day7 where

import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as T

import           Data.Char                      ( digitToInt )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )


day7 :: T.Text -> T.Text
day7 t = mconcat
  [ "Result Day 7 - first part: "
  , T.pack . show $ day7part1 t "shiny gold bag"
  , "\n"
  , "Result Day 7 - second part: "
  , T.pack . show $ day7part2 t "shiny gold bag"
  ]

day7part1 :: T.Text -> T.Text -> Int
day7part1 t = length . Set.fromList . traverseGraph graph
  where graph = parseGraph Map.empty . T.lines $ t

day7part2 :: T.Text -> T.Text -> Int
day7part2 _ _ = 1

-- ==================================================================
-- Data, classes, types and static values
-- ==================================================================
data Node = Node {
    name :: T.Text,
    vertices :: [Vertex]} | Nil deriving (Show, Eq)
instance Ord Node where
  n1 <= n2 = name n1 <= name n2

addVertex :: Node -> Vertex -> Node
addVertex n v | v `elem` vertices n = n
              | otherwise           = Node (name n) (v : vertices n)

data Vertex = Directed {
     to :: Node
    , cap :: Int
} | Undirected {
     to :: Node
    , cap :: Int
} deriving (Show, Eq)

buildVertex :: Bool -> Node -> Int -> Vertex
buildVertex dir = if dir then Directed else Undirected

type InvertedIndex = Map.Map T.Text [Node]
-- ==================================================================
-- Functions
-- ==================================================================
parseNode :: T.Text -> Node
parseNode "no other bags" = Nil
parseNode t               = Node (T.dropWhileEnd (== 's') . T.strip $ t) []

parseVertex :: T.Text -> Vertex
parseVertex "no other bags" = buildVertex True Nil 0
parseVertex t               = buildVertex True node capacity
 where
  stripped = T.strip t
  capacity = digitToInt . T.head $ stripped
  node     = parseNode . T.tail $ stripped

-- | Parse a line, in the form of: 
--     - "light red bags contain 1 bright white bag, 2 muted yellow bags."
--     - "bright white bags contain 1 shiny gold bag."
--     - "faded blue bags contain no other bags."
parseLine :: T.Text -> Node
parseLine t = foldl addVertex node (reverse vertices)
 where
  parts = T.splitOn "contain" . T.dropWhileEnd (== '.') $ t
  node  = parseNode . head $ parts
  vertices =
    map (parseVertex . T.strip) . concatMap (T.splitOn ",") $ tail parts

parseGraph :: InvertedIndex -> [T.Text] -> InvertedIndex
parseGraph = foldl addLine

insertNode :: InvertedIndex -> Node -> InvertedIndex
insertNode i n | isNothing (Map.lookup nodeName i) = Map.insert nodeName [] i
               | otherwise                         = i
  where nodeName = name n

addParent :: InvertedIndex -> Node -> Node -> InvertedIndex
addParent i _ Nil = i
addParent i p c | isNothing ps = let i' = insertNode i c in addParent i' p c
                | otherwise    = Map.insert nodeName ps' i
 where
  nodeName = name c
  ps       = Map.lookup nodeName i
  ps'      = maybe [p] (p :) ps

addLine :: InvertedIndex -> T.Text -> InvertedIndex
addLine i t = foldl (`addParent` node) i $ map to . vertices $ node
  where node = parseLine t

traverseGraph :: InvertedIndex -> T.Text -> [Node]
traverseGraph i sv
  | isNothing res = []
  | otherwise     = parents ++ concatMap (traverseGraph i . name) parents
 where
  res     = Map.lookup sv i
  parents = fromJust res
