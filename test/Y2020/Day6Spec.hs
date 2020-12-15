{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day6Spec
  ( spec
  ) where

import qualified Data.Text as T

import           Test.Hspec

import Y2020.Day6

-- ===================================================================
-- Static test values
-- ===================================================================
exampleInput :: T.Text
exampleInput = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"

exampleInputGrouped :: [Group]
exampleInputGrouped = [["abc"], ["a", "b", "c"], ["ab", "ac"], ["a", "a", "a", "a"], ["b"]]
-- ===================================================================
--  Specification
-- ===================================================================
spec :: Spec
spec = do
  describe "day6" $ do
    it "should return the correct values on the test input" $ do
      day6part1 exampleInputGrouped `shouldBe` 11
      day6part2 exampleInputGrouped `shouldBe` 6

    describe "parseGroups" $ do
      it "should parse the example input into groups, seperated by an empty line" $ do
        parseGroups exampleInput `shouldBe` exampleInputGrouped

    describe "unionAnswers" $ do
      it "should count the distinct number of 'yes' answers in a group" $ do
        map unionAnswers exampleInputGrouped `shouldBe` [3,3,3,1,1]
        (unionAnswers . (:[]) . T.pack $ ['a'..'z'])  `shouldBe` 26

    describe "intersectAnswers" $ do
      it "should count the number of 'yes' answers occurring in all answers in a group" $ do
        map intersectAnswers exampleInputGrouped `shouldBe` [3,0,1,1,1]
        (intersectAnswers . (:[]) . T.pack $ ['a'..'z'])  `shouldBe` 26