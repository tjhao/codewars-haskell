module CaesarSort where

import Data.List
import Data.Char

caesarSort :: [String] -> [[String]]
caesarSort = go []

go :: [[String]] -> [String] -> [[String]]
go recs [] = recs
go recs (x:xs) = go (fs:recs) rs
    where fs = x:filter (isSame x) xs
          rs = [x | x <- xs, x `notElem` fs]

caesar :: Int -> String -> String
caesar n = map (\c -> chr . (\x -> 97 + (x - 97 + n) `mod` 26) $ ord c)

isSame :: String -> String -> Bool
isSame s1 s2 = s1 `elem` map (flip caesar s2) [1..26]
