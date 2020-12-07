{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day2 where

import qualified Data.Text                     as T

import           Text.Read                      ( readMaybe )
import           Control.Applicative            ( Applicative(liftA2) )

import           Utils                          ( openFile
                                                , printToOutput
                                                )
import           Data.Bits                      ( Bits(xor) )
import           Data.Maybe                     ( fromMaybe )

day2 :: IO ()
day2 = do
  inputs <- openFile "resources/2020/day2.txt"
  let records  = map parseRecord (T.lines inputs)
  let results1 = map checkRecord records
  let results2 = map checkRecord' records
  printToOutput
    (mconcat
      [ "Result Day 2 - first part: "
      , T.pack . show . countTrues $ results1
      ]
    )
  printToOutput
    (mconcat
      [ "Result Day 2 - second part: "
      , T.pack . show . countTrues $ results2
      ]
    )

-- ===================================================
--  Data, types and static values
-- ===================================================
data Record = Record
  { minOcc    :: Maybe Int
  , maxOcc    :: Maybe Int
  , checkChar :: Char
  , password  :: T.Text
  }
  deriving Show

testValues :: [T.Text]
testValues =
  ["11-16 l: llllqllllllllflq", "3-6 q: qvqqqpzqd", "1-6 l: lgljslmsmbwbllsjw"]

testRecords :: [Record]
testRecords = map parseRecord testValues

-- ===================================================
--  Functions
-- ===================================================

parseRecord :: T.Text -> Record
parseRecord t = Record { minOcc    = minOcc'
                       , maxOcc    = maxOcc'
                       , checkChar = checkChar'
                       , password  = password'
                       } where
  (minOcc', maxOcc') = parseOccurrances . head $ splitText
  checkChar'         = T.head (splitText !! 1)
  password'          = last splitText
  splitText          = T.words t

checkRecord :: Record -> Bool
checkRecord (Record Nothing _       _ _ ) = False
checkRecord (Record _       Nothing _ _ ) = False
checkRecord (Record _       _       _ "") = False
checkRecord (Record minOcc maxOcc checkChar password) =
  let mMin     = (T.length checkStr >=) <$> minOcc
      mMax     = (T.length checkStr <=) <$> maxOcc
      checkStr = T.filter (checkChar ==) password
  in  fromMaybe False (liftA2 (&&) (liftA2 (&&) (Just True) mMin) mMax)

checkRecord' :: Record -> Bool
checkRecord' (Record Nothing _       _ _ ) = False
checkRecord' (Record _       Nothing _ _ ) = False
checkRecord' (Record _       _       _ "") = False
checkRecord' (Record minOcc maxOcc checkChar password) =
  let mMin = checkCharOnIndex password checkChar <$> minOcc
      mMax = checkCharOnIndex password checkChar <$> maxOcc
  in  fromMaybe False (liftA2 xor (liftA2 xor (Just False) mMin) mMax)

checkCharOnIndex :: T.Text -> Char -> Int -> Bool
checkCharOnIndex t cc i = cc == T.index t (i - 1)

parseOccurrances :: T.Text -> (Maybe Int, Maybe Int)
parseOccurrances t = (minOcc, maxOcc)
 where
  (minOccText : rest) = T.splitOn "-" t
  parseMaybeInt       = readMaybe . T.unpack . T.strip
  minOcc              = parseMaybeInt minOccText
  maxOcc              = parseMaybeInt . head . T.splitOn " " $ head rest


countTrues :: [Bool] -> Int
countTrues = foldl (\c b -> fromEnum b + c) 0