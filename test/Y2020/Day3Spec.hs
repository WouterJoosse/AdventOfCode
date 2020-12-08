{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day3Spec
  ( spec
  ) where

import           Test.Hspec
import qualified Data.Text                     as T
import           Y2020.Day3
import Control.Exception (evaluate)

-- ====================================================================
--  Mock stuff
-- ====================================================================
testMapLineText :: T.Text
testMapLineText = "..##......."

testMapLineText2 :: T.Text
testMapLineText2 = "#...#..#..#"

testMapLine :: [Field]
testMapLine = [Open, Open, Tree, Tree, Open, Open, Open, Open, Open, Open, Open]

testMapLine2 :: [Field]
testMapLine2 = [Tree, Open, Open, Open, Tree, Open, Open, Tree, Open, Open, Tree]

testMapSection :: MapSection
testMapSection = [testMapLine, testMapLine2]

testMapSection2 :: MapSection
testMapSection2 = [testMapLine2, testMapLine]

testMap :: Map
testMap = [testMapSection, testMapSection2]

exampleMapInput :: T.Text
exampleMapInput =
  "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"

exampleMapSectionInput :: MapSection
exampleMapSectionInput = sectionTextToMapSection (T.words exampleMapInput)

exampleMap :: Map
exampleMap = repeat exampleMapSectionInput

-- ====================================================================
--  Spec
-- ====================================================================
spec :: Spec
spec = do
  describe "Day3" $ do
    it "should find the provided outcomes on the provided example map, slopes and start positions" $ do
      countTrees (buildPath exampleMap (1,1) (0, 0)) `shouldBe` 2
      countTrees (buildPath exampleMap (3,1) (0, 0)) `shouldBe` 7
      countTrees (buildPath exampleMap (5,1) (0, 0)) `shouldBe` 3
      countTrees (buildPath exampleMap (7,1) (0, 0)) `shouldBe` 4
      countTrees (buildPath exampleMap (1,2) (0, 0)) `shouldBe` 2

    describe "convertMapLine" $ do
      it "parses a line to a list of Fields" $ do
        convertMapLine testMapLineText `shouldBe` testMapLine 
        convertMapLine testMapLineText2 `shouldBe` testMapLine2
      it "yields the same amount of fields as characters in the input line"
        $ do
            (length . convertMapLine $ testMapLineText)
              `shouldBe` length testMapLine
            (length . convertMapLine $ testMapLineText2)
              `shouldBe` length testMapLine2

    describe "sectionTextToMapSection" $ do
      it "parses a list of input lines to a MapSection" $ do
        sectionTextToMapSection [testMapLineText] `shouldBe` [testMapLine]

    describe "getField" $ do
      context "when the x value is longer than the width of a section" $ do
        it "should return a value from the next section" $ do
            getField (15,0) testMap `shouldBe` Tree
      context "when the y value is longer than the height of a section" $ do
        it  "should return Nothing" $ do
            evaluate (getField (16, 3) testMap) `shouldThrow` errorCall "invalid coördinates provided!"

    describe "countTrees" $ do
      it "counts the correct number of trees" $ do
        countTrees testMapLine `shouldBe` 2
        countTrees testMapLine2 `shouldBe` 4
        countTrees (buildPath testMap (1,1) (3,0)) `shouldBe` 2 

    describe "buildPath" $ do
        it "should yield a list of Fields" $ do
            buildPath testMap (0,1) (0,0) `shouldBe` [Open, Tree]
            buildPath testMap (1,1) (3,0) `shouldBe` [Tree, Tree]
        
