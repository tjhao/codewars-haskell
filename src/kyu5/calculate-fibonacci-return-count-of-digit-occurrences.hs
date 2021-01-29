module Kata (fibDigits) where

import Data.Char
import Data.List
import Data.Ord

fibDigits :: Integer -> [(Integer, Integer)]
fibDigits n = getTimes (fibs !! fromInteger n)

fibs :: [Integer]
fibs = fib 0 1 where fib a b = a:fib b (a + b)

getTimes :: Integer -> [(Integer, Integer)]
getTimes = sortBy (comparing Down) . map ((,) <$> (toInteger . length) <*> (toInteger . digitToInt . head)) . group . sort . show
