module LastDigit where

import Data.List

lastDigit :: Integer -> Integer -> Integer
lastDigit _ 0 = 1
lastDigit a b = lastDigits !! (fromInteger (b - 1) `mod` length lastDigits)
    where lastDigits = (a `mod` 10) : unfoldr (\n -> if n == (a `mod` 10) then Nothing else Just (n, (n * a) `mod` 10)) ((a ^ 2) `mod` 10)
