module MiddlePermutation.JorgeVS.Kata where

import Data.List

middlePermutation :: String -> String
middlePermutation xs = let pList = sort $ permutations xs; len = length pList in 
                           if even len then pList !! (len `div` 2 - 1) else pList !! (len `div` 2)
