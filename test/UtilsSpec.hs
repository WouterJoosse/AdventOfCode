{-# LANGUAGE OverloadedStrings #-}
module UtilsSpec
  ( spec
  ) where

import qualified Data.Text                     as T

import           Test.Hspec
import           Utils                          ( splitInputOnEmptyLine )



exampleInput1 :: T.Text
exampleInput1 = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"

exampleInput2 :: T.Text
exampleInput2 =
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"


splitInput1 :: [[T.Text]]
splitInput1 = [["abc"], ["a", "b", "c"], ["ab", "ac"], ["a", "a", "a", "a"], ["b"]]

splitInput2 :: [[T.Text]]
splitInput2 =
  [ [ "ecl:gry"
    , "pid:860033327"
    , "eyr:2020"
    , "hcl:#fffffd"
    , "byr:1937"
    , "iyr:2017"
    , "cid:147"
    , "hgt:183cm"
    ]
  , [ "iyr:2013"
    , "ecl:amb"
    , "cid:350"
    , "eyr:2023"
    , "pid:028048884"
    , "hcl:#cfa07d"
    , "byr:1929"
    ]
  , [ "hcl:#ae17e1"
    , "iyr:2013"
    , "eyr:2024"
    , "ecl:brn"
    , "pid:760753108"
    , "byr:1931"
    , "hgt:179cm"
    ]
  , [ "hcl:#cfa07d"
    , "eyr:2025"
    , "pid:166559648"
    , "iyr:2011"
    , "ecl:brn"
    , "hgt:59in"
    ]
  ]



spec :: Spec
spec = do
  describe "splitInputOnEmptyLine" $ do
    it "should split input into a list of texts on empty lines" $ do
      splitInputOnEmptyLine exampleInput1 `shouldBe` splitInput1
      splitInputOnEmptyLine exampleInput2 `shouldBe` splitInput2
