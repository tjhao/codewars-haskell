module Change where

import Data.Array

countChange :: Integer -> [Integer] -> Integer
countChange m list = table ! (m, 1)
    where
        n = length list

        a :: Array Int Integer
        a = listArray (1, n) list

        table :: Array (Integer, Int) Integer
        table = listArray ((0, 1), (m, n)) [f i j | i <- [0..m], j <- [1..n]]
        
        decide i x | i `mod` x == 0 = 1
                   | otherwise      = 0

        f i j | i == 0    = 1
              | j == n    = decide i (a!n)
              | i < (a!j) = table!(i, j+1)
              | otherwise = table!(i, j+1) + table!(i-a!j, j)
