{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day4Spec
  ( spec
  ) where

import qualified Data.Text                     as T

import           Test.Hspec
import           Y2020.Day4


-- ===================================================================
--  Static mock stuff
-- ===================================================================

exampleInput :: T.Text
exampleInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

examplePassportValidFields :: PassportValues
examplePassportValidFields = ["ecl:gry","pid:860033327","eyr:2020","hcl:#fffffd","byr:1937","iyr:2017","cid:147","hgt:183cm"]

examplePassportValid :: Passport
examplePassportValid = Passport (Just 1937)
                                (Just 2017)
                                (Just 2020)
                                (Just "183cm")
                                (Just "#fffffd")
                                (Just "gry")
                                (Just 860033327)
                                (Just 147)

examplePassportInvalidByrFields:: PassportValues
examplePassportInvalidByrFields = ["ecl:gry","pid:860033327","eyr:2020","hcl:#fffffd","iyr:2017","cid:147","hgt:183cm"]

examplePassportInValidByr :: Passport
examplePassportInValidByr = Passport Nothing
                                     (Just 2017)
                                     (Just 2020)
                                     (Just "183cm")
                                     (Just "#fffffd")
                                     (Just "gry")
                                     (Just 860033327)
                                     (Just 147)

examplePassportInValidIyrFields :: PassportValues
examplePassportInValidIyrFields = ["ecl:gry","pid:860033327","eyr:2020","hcl:#fffffd","byr:1937","cid:147","hgt:183cm"]

examplePassportInValidIyr :: Passport
examplePassportInValidIyr = Passport (Just 1937)
                                     Nothing
                                     (Just 2020)
                                     (Just "183cm")
                                     (Just "#fffffd")
                                     (Just "gry")
                                     (Just 860033327)
                                     (Just 147)

exampleNorthPoleCredentialFields :: PassportValues
exampleNorthPoleCredentialFields = ["hcl:#ae17e1","iyr:2013","eyr:2024","ecl:brn","pid:760753108","byr:1931","hgt:179cm"]

exampleNorthPoleCredential :: Passport
exampleNorthPoleCredential = Passport (Just 1931)
                                      (Just 2013)
                                      (Just 2024)
                                      (Just "179cm")
                                      (Just "#ae17e1")
                                      (Just "brn")
                                      (Just 760753108)
                                      Nothing

-- ===================================================================
--  Spec
-- ===================================================================
spec :: Spec
spec = do
  describe "Day 4" $ do
    it "should return 2 on the example input" $ do
      (countValid . map parsePassport . splitPassportInputs $ exampleInput)
        `shouldBe` 2

    describe "splitPassportInputs" $ do
      it "should split input on empty lines" $ do
        (length . splitPassportInputs $ exampleInput) `shouldBe` 4

    describe "parsePassport" $ do
      it "should fill all provided values in the passport" $ do
        parsePassport examplePassportValidFields `shouldBe` examplePassportValid
      it "should fill missing values with Nothing" $ do
        parsePassport examplePassportInvalidByrFields
          `shouldBe` examplePassportInValidByr
        parsePassport examplePassportInValidIyrFields
          `shouldBe` examplePassportInValidIyr
        parsePassport exampleNorthPoleCredentialFields
          `shouldBe` exampleNorthPoleCredential

    describe "parseDataField" $ do
      it "should fill a missing value in a passport" $ do
        parseDataField "pid:760753108" emptyPassport `shouldBe` emptyPassport { pid = Just 760753108}
        parseDataField "iyr:2013" emptyPassport `shouldBe` emptyPassport { iyr = Just 2013}

    describe "isvalid" $ do
      it "should reject passports that have do not have required fields" $ do
        isvalid examplePassportInValidByr `shouldBe` Invalid
        isvalid examplePassportInValidIyr `shouldBe` Invalid
      it "should validate passport containing all required fields" $ do
        isvalid examplePassportValid `shouldBe` Valid
      it "should possibly validate north pole credentials" $ do
        isvalid exampleNorthPoleCredential `shouldBe` PossibleValid

    describe "countValid" $ do
      it "should not count invalid passports" $ do
        countValid [examplePassportInValidByr, examplePassportInValidIyr]
          `shouldBe` 0
      it "should count North Pole Credentials" $ do
        countValid [exampleNorthPoleCredential] `shouldBe` 1
      it "should count valid passports" $ do
        countValid [examplePassportValid] `shouldBe` 1
