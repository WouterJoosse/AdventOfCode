{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day4 where

import qualified Data.Text as T

import Data.Ix (inRange)
import Utils (openFile, printToOutput)

day4 :: IO ()
day4 = do
  input <- openFile "resources/2020/day4.txt"
  let pVals = splitPassportInputs input
  let passports = map parsePassport pVals
  printToOutput (mconcat ["Result Day 4 - first part: ", T.pack . show . countValid $ passports]) 
  printToOutput (mconcat ["Result Day 4 - second part: ", ""]) 
  

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
  , pid :: Maybe T.Text
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
                   | k == "pid" = p {pid = Just v}
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

byrValidation :: Int -> Bool
byrValidation = inRange (1920, 2002) 

iyrValidation :: Int -> Bool
iyrValidation = inRange (2010, 2020)

eyrValidation :: Int -> Bool
eyrValidation  = inRange (2020, 2030)

hgtValidation :: T.Text -> Bool
hgtValidation s = inrange v uom
  where inrange v uom | uom == "cm" = inRange (150, 193) v
                      | uom == "in" = inRange (59, 76) v
                      | otherwise = False
        uom = T.reverse . T.take 2 . T.reverse $ s
        v = read . T.unpack . T.dropEnd 2 $ s

hclValidation :: T.Text -> Bool
hclValidation s = (start == '#') && whiteList rest
  where start = T.head s
        rest = T.unpack . T.tail $ s
        whiteList [] = True
        whiteList (x:xs) = x `elem` (['a'..'f'] ++ ['0'..'9']) && whiteList xs

eclValidation :: T.Text -> Bool
eclValidation t = t `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pidValidation :: T.Text -> Bool
pidValidation t = T.length t == 9 && T.all (`elem` ['0'..'9']) t

-- | Count the number of valid passports in a list of passports
countValid :: [Passport] -> Int
countValid = length . filter ( Invalid /=) . map isvalid