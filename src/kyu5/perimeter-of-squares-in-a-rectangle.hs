module Codewars.Kata.Perimeter where

perimeter :: Integer -> Integer
perimeter n = (4*) . sum $ take (fromInteger n + 1) fibs
  
fibs :: [Integer]
fibs = fib 1 1 where fib a b = a:fib b (a+b)
