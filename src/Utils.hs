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
