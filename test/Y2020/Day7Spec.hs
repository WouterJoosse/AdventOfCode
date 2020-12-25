{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day7Spec where

import qualified Data.Text                     as T
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Y2020.Day7


-- ===================================================================
-- Static test values
-- ===================================================================
exampleInput :: T.Text
exampleInput =
  "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags."

exampleInputLines :: [T.Text]
exampleInputLines = T.lines exampleInput

exampleInput2 :: T.Text
exampleInput2 = 
  "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags."

exampleInputLines2 :: [T.Text]
exampleInputLines2 = T.lines exampleInput2


-- ===================================================================
--  Specification
-- ===================================================================
spec :: Spec
spec = do
  let line1 = "light red bags contain 1 bright white bag, 2 muted yellow bags."
  let line2 = "bright white bags contain 1 shiny gold bag."
  let line3 = "dotted black bags contain no other bags."
  let node1 = Node "light red bag" []
  let node2 = Node "bright white bag" []
  let node3 = Node "faded blue bag" []
  let node4 = Node "muted yellow bag" []
  let node5 = Node "shiny gold bag" []
  let node6 = Node "dotted black bag" []

  describe "day7" $ do
    it "should return the correct values on the test input" $ do
      day7part1 exampleInput "shiny gold bag" `shouldBe` 4
      -- day7part2 exampleInput "shiny gold bag" `shouldBe` 32
      -- day7part2 exampleInput2 "shiny gold bag" `shouldBe` 126

    describe "parseNode" $ do
      it "should parse the name of the current bag, as singular value" $ do
        parseNode "light red bags" `shouldBe` node1
        parseNode "bright white bags" `shouldBe` node2
        parseNode "faded blue bags" `shouldBe` node3
        parseNode "no other bags" `shouldBe` Nil

    describe "parseVertex" $ do
      it "should parse the name of the bag, as singular value" $ do
        parseVertex "2 muted yellow bags" `shouldBe` Directed node4 2
      it "should parse the number of the bags as integer" $ do
        parseVertex "1 shiny gold bag" `shouldBe` Directed node5 1
      it
          "should return a Nil node with no capactity if the bag can't contain other bags"
        $ do
            parseVertex "no other bags" `shouldBe` Directed Nil 0

    describe "buildVertex" $ do
      it "should build the contents of a line to a vertex" $ do
        buildVertex True node2 3 `shouldBe` Directed node2 3
        buildVertex True Nil 0 `shouldBe` Directed Nil 0
        buildVertex False node2 3 `shouldBe` Undirected node2 3

    describe "parseLine" $ do
      it "should return a Node and the outgoing vertices" $ do
        parseLine line1
          `shouldBe` Node "light red bag" [Directed node2 1, Directed node4 2]
        parseLine line2 `shouldBe` Node "bright white bag" [Directed node5 1]
        parseLine line3 `shouldBe` Node "dotted black bag" [Directed Nil 0]




