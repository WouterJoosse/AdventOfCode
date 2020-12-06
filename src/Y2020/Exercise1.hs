{-# LANGUAGE OverloadedStrings #-}
module Y2020.Exercise1 
where

import Utils (openFile, printFile)

exercise1 :: IO ()
exercise1 = do
    contents <- openFile "resources/2020/exercise1.txt"
    printFile contents
