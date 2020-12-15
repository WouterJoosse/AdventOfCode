{-# LANGUAGE OverloadedStrings #-}
module Utils where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

{-| 
    Open a file and return its contents
-}
openFile :: T.Text -> IO T.Text
openFile = TIO.readFile . T.unpack

{-|
    Print to STD
-}
printToOutput :: T.Text -> IO ()
printToOutput = TIO.putStrLn

-- | Split input on empty lines into lists of text
splitInputOnEmptyLine :: T.Text -> [[T.Text]]
splitInputOnEmptyLine t = 
    let inputLines = T.splitOn "\n\n" t
    in map T.words inputLines