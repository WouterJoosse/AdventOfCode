{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day3 where

import qualified Data.Text                     as T

day3 :: T.Text -> T.Text
day3 input =
  let mapSection = sectionTextToMapSection (T.lines input)
      map        = repeat mapSection
      path1      = countTrees . buildPath map (1, 1) $ (0, 0)
      path2      = countTrees . buildPath map (3, 1) $ (0, 0)
      path3      = countTrees . buildPath map (5, 1) $ (0, 0)
      path4      = countTrees . buildPath map (7, 1) $ (0, 0)
      path5      = countTrees . buildPath map (1, 2) $ (0, 0)
  in  mconcat
        [ "Result Day 3 - first part: "
        , T.pack . show $ path2
        , "\n"
        , "Result Day 3 - second part: "
        , T.pack . show $ path1 * path2 * path3 * path4 * path5
        ]


-- ===================================================
--  Data, types and static values
-- ===================================================
data Field = Tree | Open deriving (Eq)
instance Show Field where
  show Tree = "#"
  show Open = "."

type MapLine = [Field]
type MapSection = [MapLine]
type Map = [MapSection]

type Slope = (Int, Int)
type Path = [Field]
type Position = (Int, Int)

-- ===================================================
--  Functions
-- ===================================================
-- | Convert an line from the input to a list of 'Field'
convertMapLine :: T.Text -> [Field]
convertMapLine "" = []
convertMapLine t  = field : convertMapLine (T.tail t)
 where
  field | T.head t == '.' = Open
        | otherwise       = Tree

-- | Convert the input to a 'MapSection'. 
sectionTextToMapSection :: [T.Text] -> MapSection
sectionTextToMapSection = map convertMapLine

-- | Given a 'Position' on a 'Map' return the corresponding 'Field'
getField :: Position -> Map -> Field
getField (x, y) m = if y >= maxY
  then error "invalid coördinates provided!"
  else ((m !! sectionNum) !! y) !! x'
 where
  maxX       = length . head . head $ m
  maxY       = length . head $ m
  sectionNum = x `div` maxX
  x'         = x `mod` maxX

-- | Given a 'Path', return the number of 'Tree's in that path
countTrees :: Path -> Int
countTrees = length . filter (== Tree)

-- | Given a 'Map', a 'Slope' and a starting position, return the 'Path'
--   to the end of the 'Map'
buildPath :: Map -> Slope -> Position -> Path
buildPath m (dx, dy) (x, y) =
  let maxY = length . head $ m
  in  if y >= maxY
        then []
        else getField (x, y) m : buildPath m (dx, dy) (x + dx, y + dy)
