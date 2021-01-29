module Codewars.Kata.Fib where

productFib :: Integer -> (Integer, Integer, Bool)
productFib n = (a, b, a * b == n)
  where
    (a, b) = head . dropWhile ((< n) . uncurry (*)) . zip fibs $ tail fibs

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)
