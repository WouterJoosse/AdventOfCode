{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day4Spec
  ( spec
  ) where

import qualified Data.Text                     as T

import           Data.List
import           Test.Hspec
import           Y2020.Day4
import           System.Random                  ( Random(randomRIO) )
import           Control.Monad                  ( replicateM )

-- ===================================================================
--  Static mock stuff
-- ===================================================================

exampleInput :: T.Text
exampleInput =
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

examplePassportValidFields :: PassportValues
examplePassportValidFields =
  [ "ecl:gry"
  , "pid:860033327"
  , "eyr:2020"
  , "hcl:#fffffd"
  , "byr:1937"
  , "iyr:2017"
  , "cid:147"
  , "hgt:183cm"
  ]

examplePassportValid :: Passport
examplePassportValid = Passport (Just 1937)
                                (Just 2017)
                                (Just 2020)
                                (Just "183cm")
                                (Just "#fffffd")
                                (Just "gry")
                                (Just "860033327")
                                (Just 147)

examplePassportInvalidByrFields :: PassportValues
examplePassportInvalidByrFields =
  [ "ecl:gry"
  , "pid:860033327"
  , "eyr:2020"
  , "hcl:#fffffd"
  , "iyr:2017"
  , "cid:147"
  , "hgt:183cm"
  ]

examplePassportInValidByr :: Passport
examplePassportInValidByr = Passport Nothing
                                     (Just 2017)
                                     (Just 2020)
                                     (Just "183cm")
                                     (Just "#fffffd")
                                     (Just "gry")
                                     (Just "860033327")
                                     (Just 147)

examplePassportInValidIyrFields :: PassportValues
examplePassportInValidIyrFields =
  [ "ecl:gry"
  , "pid:860033327"
  , "eyr:2020"
  , "hcl:#fffffd"
  , "byr:1937"
  , "cid:147"
  , "hgt:183cm"
  ]

examplePassportInValidIyr :: Passport
examplePassportInValidIyr = Passport (Just 1937)
                                     Nothing
                                     (Just 2020)
                                     (Just "183cm")
                                     (Just "#fffffd")
                                     (Just "gry")
                                     (Just "860033327")
                                     (Just 147)

exampleNorthPoleCredentialFields :: PassportValues
exampleNorthPoleCredentialFields =
  [ "hcl:#ae17e1"
  , "iyr:2013"
  , "eyr:2024"
  , "ecl:brn"
  , "pid:760753108"
  , "byr:1931"
  , "hgt:179cm"
  ]

exampleNorthPoleCredential :: Passport
exampleNorthPoleCredential = Passport (Just 1931)
                                      (Just 2013)
                                      (Just 2024)
                                      (Just "179cm")
                                      (Just "#ae17e1")
                                      (Just "brn")
                                      (Just "760753108")
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
        parseDataField "pid:760753108" emptyPassport
          `shouldBe` emptyPassport { pid = Just "760753108" }
        parseDataField "iyr:2013" emptyPassport
          `shouldBe` emptyPassport { iyr = Just 2013 }

    describe "isvalid" $ do
      it "should reject passports that have do not have required fields" $ do
        isvalid examplePassportInValidByr `shouldBe` Invalid
        isvalid examplePassportInValidIyr `shouldBe` Invalid
      it "should validate passport containing all required fields" $ do
        isvalid examplePassportValid `shouldBe` Valid
      it "should possibly validate north pole credentials" $ do
        isvalid exampleNorthPoleCredential `shouldBe` PossibleValid

    describe "isvalid'" $ do
      it "should validate passports with valid values" $ do
        let validNPC= parsePassport . T.words $ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"
        let invalidPassport = parsePassport . T.words $ "iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946"
        let validPassport = parsePassport . T.words $ "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
        isvalid' validNPC `shouldBe` PossibleValid
        isvalid' invalidPassport `shouldBe` Invalid
        isvalid' validPassport `shouldBe` Valid

    describe "countValid" $ do
      it "should not count invalid passports" $ do
        countValid [examplePassportInValidByr, examplePassportInValidIyr]
          `shouldBe` 0
      it "should count North Pole Credentials" $ do
        countValid [exampleNorthPoleCredential] `shouldBe` 1
      it "should count valid passports" $ do
        countValid [examplePassportValid] `shouldBe` 1

    describe "countValid'" $ do
      it "should not count invalid passports" $ do
        let invalidPassport = parsePassport . T.words $ "iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946"
        let invalidPassports = [invalidPassport, examplePassportInValidByr]
        countValid' invalidPassports `shouldBe` 0
      it "should count North Pole Credentials" $ do
        let validNPC= parsePassport . T.words $ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"
        countValid [exampleNorthPoleCredential, validNPC] `shouldBe` 2
      it "should count valid passports" $ do
        let validPassport = parsePassport . T.words $ "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
        countValid [examplePassportValid, validPassport] `shouldBe` 1

    describe "byrValidation" $ do
      it "should validate the correct years" $ do
        byrValidation 1919 `shouldBe` False
        byrValidation 2003 `shouldBe` False
        all byrValidation [1920 .. 2002] `shouldBe` True

    describe "iyrValidation" $ do
      it "should validate the correct years" $ do
        iyrValidation 2009 `shouldBe` False
        iyrValidation 2021 `shouldBe` False
        all iyrValidation [2010 .. 2020] `shouldBe` True

    describe "eyrValidation" $ do
      it "should validate the correct years" $ do
        eyrValidation 2019 `shouldBe` False
        eyrValidation 2031 `shouldBe` False
        all eyrValidation [2020 .. 2030] `shouldBe` True

    describe "hgtValidation" $ do
      it "should validate the correct heights" $ do
        hgtValidation "104cm" `shouldBe` False
        hgtValidation "194cm" `shouldBe` False
        hgtValidation "58in" `shouldBe` False
        hgtValidation "77in" `shouldBe` False
        hgtValidation "193in" `shouldBe` False
        hgtValidation "150in" `shouldBe` False
        hgtValidation "59cm" `shouldBe` False
        hgtValidation "76cm" `shouldBe` False
        let cmHeights = [150 .. 193]
        let inHeights = [59 .. 76]
        let cms = map (T.pack . \h -> show h ++ "cm") cmHeights
        let ins = map (T.pack . \i -> show i ++ "in") inHeights
        all hgtValidation cms `shouldBe` True
        all hgtValidation ins `shouldBe` True

    describe "hclValidation" $ do
      it "should validate the correct colors" $ do
        let black = "#000000"
        let white = "#ffffff"
        hclValidation black `shouldBe` True
        hclValidation white `shouldBe` True
      it "should not validate incorrect colors" $ do
        -- Far from complete...
        let invalid1 = "000000"
        let invalid2 = "#g00000"
        hclValidation invalid1 `shouldBe` False
        hclValidation invalid2 `shouldBe` False

    describe "eclValidation" $ do
      it "should validate the correct colors" $ do
        let validColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        all eclValidation validColors `shouldBe` True
      it "should not validate incorrect colors" $ do
        -- Far from complete...
        let invalidColors = generateInvalidEyeColors
              ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        all eclValidation invalidColors `shouldBe` False

    describe "pidValidation" $ do
      it "should validate only numbers of length 9" $ do
        validNumbers <- generateValidPIDNumbers
        all pidValidation validNumbers `shouldBe` True
        -- Far from complete...
        invalidNumbers <- generateIncorrectPIDNumbers validNumbers
        all pidValidation invalidNumbers `shouldBe` False

-- ============================================================
-- Helper functions
-- ============================================================
generateInvalidEyeColors :: [T.Text] -> [T.Text]
generateInvalidEyeColors validColors = filter (`notElem` validColors) colors
  where colors = map (T.pack . take 3) . permutations $ ['a' .. 'z']

generateValidPIDNumbers :: IO [T.Text]
generateValidPIDNumbers = addPrefix <$> numbers
 where
  addPrefix num = T.justifyRight 9 '0' . (T.pack . show) <$> num
  numbers = generateNRandomNumbers 10 (1, 999999999)

generateIncorrectPIDNumbers :: [T.Text] -> IO [T.Text]
generateIncorrectPIDNumbers validNumbers =
  filter (`notElem` validNumbers) <$> numbers
 where
  numbers = map (T.pack . show) <$> generateNRandomNumbers 1000 (1, 999999999)

generateNRandomNumbers :: Int -> (Int, Int) -> IO [Int]
generateNRandomNumbers n r = do
  replicateM n $ randomRIO r
