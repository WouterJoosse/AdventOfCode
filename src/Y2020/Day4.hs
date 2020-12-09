module Y2020.Day4 where

import qualified Data.Text as T

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

-- ===================================================
--  Functions
-- ===================================================
splitPassportInputs :: T.Text -> [T.Text]
splitPassportInputs = undefined

parsePassport :: T.Text -> Passport
parsePassport = undefined

isvalid :: Passport -> PassportVerdict
isvalid = undefined

countValid :: [Passport] -> Int
countValid = length . filter ( Invalid /=) . map isvalid