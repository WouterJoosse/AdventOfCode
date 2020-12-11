{-# LANGUAGE OverloadedStrings #-}
module Y2020.Day5Spec
  ( spec
  ) where

import qualified Data.Text                     as T

import           Y2020.Day5                     ( generateId
                                                , maxSeat
                                                , parseRowNum
                                                , parseSeat
                                                , parseSeatNum
                                                , halveRange
                                                , Seat(Seat, row, seat)
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

exampleInputs :: [T.Text]
exampleInputs = ["FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"]

exampleInputSeats :: [Seat]
exampleInputSeats =
  [Seat 44 5 357, Seat 70 7 567, Seat 14 7 119, Seat 102 4 820]

spec :: Spec
spec = do
  describe "day5" $ do
    describe "parseSeat" $ do
      it "should return the correct values on the test input" $ do
        map parseSeat exampleInputs `shouldBe` exampleInputSeats

    describe "generateId" $ do
      it "should calculate the ID based on the seat and row" $ do
        generateId 44 5 `shouldBe` 357
        generateId 1 1 `shouldBe` 9
        generateId 0 0 `shouldBe` 0

    describe "parseRowNum" $ do
      it "should parse row numbers correctly" $ do
        map parseRowNum exampleInputs `shouldBe` map row exampleInputSeats

    describe "parseSeatNum" $ do
      it "should parse seat numbers correctly" $ do
        map parseSeatNum exampleInputs `shouldBe` map seat exampleInputSeats

    describe "maxSeat" $ do
      it "should return the seat with the highest ID" $ do
        foldl maxSeat (Seat 0 0 0) exampleInputSeats `shouldBe` Seat 102 4 820

    describe "halveRange" $ do
        it "should divide a range in two and return the correct half" $ do
            halveRange (0, 127) 'F' `shouldBe` (0,63)
            halveRange (0, 127) 'B' `shouldBe` (64,127)
            halveRange (40, 47) 'F' `shouldBe` (40,43)
            halveRange (40, 47) 'B' `shouldBe` (44,47)
            halveRange (44, 45) 'F' `shouldBe` (44,44)
            halveRange (44, 45) 'F' `shouldBe` (45,45)
            halveRange (44, 44) 'F' `shouldBe` (44,44)
            halveRange (0, 127) 'R' `shouldBe` (64,127)
            halveRange (1, 3) 'L' `shouldBe` (1,1)
            

