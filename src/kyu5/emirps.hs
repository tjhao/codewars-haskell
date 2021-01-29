module Codewars.G964.Emirps where
    
findEmirp :: Integer -> [Integer]
findEmirp n = if null targ then [] else [toInteger $ length targ, last targ, sum targ]
    where targ = filter (\x -> isPrime x && notPalindromic x && isRPrime x) [2..n]

isPrime :: Integer -> Bool
isPrime x = all (\y ->  x `mod` y /= 0) (takeWhile (\y ->  y*y <= x) [2..])

notPalindromic :: Integer -> Bool
notPalindromic = (/=) <$> show <*> (reverse . show)

isRPrime :: Integer -> Bool
isRPrime = isPrime . read . reverse . show
