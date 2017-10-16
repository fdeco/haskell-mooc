module Compositions where

twicePlusOne :: [Int] -> [Int]
twicePlusOne = map ((+1) . (*2))

sumFoldl :: [Int] -> Int
sumFoldl = foldl (+) 0