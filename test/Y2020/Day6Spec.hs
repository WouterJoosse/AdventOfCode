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

exampleInputGrouped :: [[T.Text]]
exampleInputGrouped = [["abc"], ["a", "b", "c"], ["ab", "ac"], ["a", "a", "a"], ["b"]]
-- ===================================================================
--  Specification
-- ===================================================================
spec :: Spec
spec = do
  describe "day6" $ do
    it "should return the correct values on the test input" $ do
      day6part1 exampleInput `shouldBe` 11

    describe "parseGroups" $ do
      it "should parse the example input into groups, seperated by an empty line" $ do
        parseGroups exampleInput `shouldBe` exampleInputGrouped