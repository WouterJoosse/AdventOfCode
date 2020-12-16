{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day7Spec where

import qualified Data.Text                     as T
import           Test.Hspec
import           Y2020.Day7


-- ===================================================================
-- Static test values
-- ===================================================================
exampleInput :: T.Text
exampleInput =
  "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags."

-- ===================================================================
--  Specification
-- ===================================================================
spec :: Spec
spec = do
  describe "day7" $ do
    it "should return the correct values on the test input" $ do
      day7part1 exampleInput `shouldBe` 4
