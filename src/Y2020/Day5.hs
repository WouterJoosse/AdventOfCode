{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day5 where

import qualified Data.Text                     as T

import           Data.List

day5 :: T.Text -> T.Text
day5 input =
  let seats      = map parseSeat (T.lines input)
      result     = foldl maxSeat (Seat 0 0 0) seats
      ids        = sort . map seatId $ seats
      missingIds = filter (`notElem` ids) [head ids .. last ids]
  in  mconcat
        [ "Result Day 5 - first part: "
        , T.pack . show . seatId $ result
        , "\n"
        , "Result Day 5 - second part: "
        , T.pack . show $ missingIds
        ]



-- =========================================
-- Data, types, classes and static values
-- =========================================
data Seat = Seat
  { row    :: Int
  , seat   :: Int
  , seatId :: Int
  }
instance Show Seat where
  show (Seat r s i) = "|" <> msg <> "|"
   where
    msg  = unwords [row, "-", seat, "-", id]
    row  = unwords ["Row:", show r]
    seat = unwords ["Seat:", show s]
    id   = unwords ["ID:", show i]
instance Eq Seat where
  (Seat _ _ id1) == (Seat _ _ id2) = id1 == id2


-- =========================================
-- Functions
-- =========================================
-- | Parse the input string and generate a seat
parseSeat :: T.Text -> Seat
parseSeat t = Seat rowNum seatNum id
 where
  rowNum  = parseRowNum t
  seatNum = parseSeatNum t
  id      = generateId rowNum seatNum

-- | Get the row number from the input text
parseRowNum :: T.Text -> Int
parseRowNum = fst . traverseBinaryRangeTree (0, 127) . T.take 7

-- | Get the seat number from the input text
parseSeatNum :: T.Text -> Int
parseSeatNum = fst . traverseBinaryRangeTree (0, 8) . T.drop 7

-- | Traverse the tree encoding, and divide the range accordingly
traverseBinaryRangeTree :: (Int, Int) -> T.Text -> (Int, Int)
traverseBinaryRangeTree r t | T.empty == t = r
                            | otherwise    = traverseBinaryRangeTree r' t'
 where
  r' = halveRange r (T.head t)
  t' = T.tail t

-- | Divide the range in two and return one of the halves, based on the
--   input character.
--   If the difference between the upper bound and lower bound of the range
--   is 0, then the range is not divided any further
halveRange :: (Int, Int) -> Char -> (Int, Int)
halveRange r c
  | snd r == fst r = r
  | otherwise = case c of
    'F' -> lowerHalf r
    'L' -> lowerHalf r
    _   -> upperHalf r
 where
  rangeUnits = (snd r - fst r) + 1
  lowerHalf (x, y) = (x, y')
   where
    y'     = y - (rangeUnits `div` 2) - offset
    offset = if even rangeUnits then 0 else 1
  upperHalf (x, y) = (x', y) where x' = x + (rangeUnits `div` 2)

-- | Calculate the ID based on the row and seat number
generateId :: Int -> Int -> Int
generateId row seat = row * 8 + seat

-- | take the max of the 'Seat's, based on the ID
maxSeat :: Seat -> Seat -> Seat
maxSeat s1 s2 = if max id1 id2 == id1 then s1 else s2
 where
  id1 = seatId s1
  id2 = seatId s2
