module Hamming where

import Control.Applicative

hamming  :: Int -> Int
hamming n = hammings !! (n-1)
    
hammings :: [Int]
hammings = 1:(merge3 (map (2*)) (map (3*)) (map (5*)) $ hammings)
    where merge3 f g h = liftA2 merge f (liftA2 merge g h)
    
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x > y     = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)
