module Codewars.G964.Johnann where

ann :: Int -> [Integer]
ann n = map toInteger $ take n ann'
john :: Int -> [Integer]
john n = map toInteger $ take n john'

sumAnn :: Int -> Integer
sumAnn = sum . ann
sumJohn :: Int -> Integer
sumJohn = sum . john

ann' :: [Int]
ann' = 1 : [ k - john' !! (ann' !! (k - 1)) | k <- [1..] ]

john' :: [Int]
john' = 0 : [ k - ann' !! (john' !! (k - 1) ) | k <- [1..] ]
