module Kata.MaximumSum (maximumSum) where

import Data.List
import Data.Ord

maximumSum :: [Int] -> [[Int]] -> Int
maximumSum xs ps = sum $ zipWith (*) indices sorted
    where indices = map snd . sortBy (comparing (Down . snd)) $ getAllTimes (length xs - 1) ps
          sorted = sortBy (comparing Down) $ xs
          
getTimes :: [(Int, Int)] -> [Int] -> [(Int, Int)]
getTimes ts p = foldl (\acc x -> map (addOne x) acc) ts [head p..last p]
    where addOne x (n, t) = if x == n then (n, t + 1) else (n, t)

getAllTimes :: Int -> [[Int]] -> [(Int, Int)]
getAllTimes n ps = foldl getTimes initTimes ps
    where initTimes = zip [0..n] [0,0..]
