{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day4 where

import qualified Data.Text as T

import Data.Maybe (isNothing)

day4 :: IO ()
day4 = do
  putStrLn "Day 4!"

-- ===================================================
--  Data, types and static values
-- ===================================================
data Passport = Passport
  { byr :: Maybe Int
  , iyr :: Maybe Int
  , eyr :: Maybe Int
  , hgt :: Maybe T.Text
  , hcl :: Maybe T.Text
  , ecl :: Maybe T.Text
  , pid :: Maybe Int
  , cid :: Maybe Int
  } deriving (Show, Eq)

data PassportVerdict = Valid | Invalid | PossibleValid deriving (Eq)
instance Show PassportVerdict where 
  show Valid = "Valid"
  show Invalid = "Invalid"
  show PossibleValid = "Possibly valid"

type PassportField = T.Text
type PassportValues = [PassportField]

emptyPassport :: Passport
emptyPassport = Passport {
  byr=Nothing, iyr=Nothing, eyr=Nothing, hgt=Nothing, hcl=Nothing, ecl=Nothing, pid=Nothing, cid=Nothing
}

-- ===================================================
--  Functions
-- ===================================================
-- | Split the input to a list of 'PassportValues'
splitPassportInputs :: T.Text -> [PassportValues]
splitPassportInputs t = map T.words splitOnTwoLineBreaks
  where splitOnTwoLineBreaks = T.splitOn "\n\n" t

-- | Parse a list of 'PassportField's to a passport
parsePassport :: PassportValues -> Passport
parsePassport t = parsePassport' t emptyPassport

parsePassport' :: PassportValues -> Passport -> Passport
parsePassport' [] p = p
parsePassport' (x:xs) p = parsePassport' xs p' 
  where p' = parseDataField x p

-- | Parse a data field by splitting the text on ":" and 
--   fill the value in the correct data field
parseDataField :: PassportField -> Passport -> Passport
parseDataField f p | k == "byr" = p {byr = Just (textToInt v)}
                   | k == "iyr" = p {iyr = Just (textToInt v)}
                   | k == "eyr" = p {eyr = Just (textToInt v)}
                   | k == "hgt" = p {hgt = Just v}
                   | k == "hcl" = p {hcl = Just v}
                   | k == "ecl" = p {ecl = Just v}
                   | k == "pid" = p {pid = Just (textToInt v)}
                   | k == "cid" = p {cid = Just (textToInt v)}
                   | otherwise = p
  where (key:rest) = T.split (== ':') f
        k = T.strip key
        textToInt = read . T.unpack
        v = head rest

-- | If a 'Passport' field is missing, then it is invalid, 
--   unless the only missing field is the 'cid' field.
-- 
--   If that is the case, then the passport is 'PossibleValid'.
isvalid :: Passport -> PassportVerdict
isvalid Passport {byr = Nothing} = Invalid
isvalid Passport {iyr = Nothing} = Invalid
isvalid Passport {eyr = Nothing} = Invalid
isvalid Passport {hgt = Nothing} = Invalid
isvalid Passport {hcl = Nothing} = Invalid
isvalid Passport {ecl = Nothing} = Invalid
isvalid Passport {pid = Nothing} = Invalid
isvalid Passport {cid = Nothing} = PossibleValid
isvalid _ = Valid

-- | Count the number of valid passports in a list of passports
countValid :: [Passport] -> Int
countValid = length . filter ( Invalid /=) . map isvalid