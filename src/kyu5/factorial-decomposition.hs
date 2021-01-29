module FactorialDecomposition.Kata (decomp) where

import Data.List

decomp :: Int -> String
decomp n = intercalate " * " . map (uncurry showP) $ getPairs n
    where showP p 1 = show p
          showP p t = show p ++ "^" ++ show t

isPrime :: Int -> Bool
isPrime x = all (\y ->  x `mod` y /= 0) (takeWhile (\y ->  y*y <= x) [2..])

getPrimes :: Int -> [Int]
getPrimes n = filter isPrime [2..n]

getTimes :: Int -> Int -> Int
getTimes n prime
    | n < prime = 0
    | otherwise = newN + getTimes newN prime where newN = n `div` prime

getPairs :: Int -> [(Int, Int)]
getPairs n = map (\p -> (p, getTimes n p)) primes 
    where primes = getPrimes n
