module Codewars.G964.Josephus where

josephusSurvivor :: Int -> Int -> Int
josephusSurvivor n k = josephus [1..n] k

josephus :: [a] -> Int -> a
josephus [x] _ = x
josephus xs k = let n = (k-1) `mod` length xs in josephus (drop (n+1) xs ++ take n xs) k
