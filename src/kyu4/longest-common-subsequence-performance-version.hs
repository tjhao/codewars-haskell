module LongestCommonSubsequence where

import Data.Array
import Data.List
import Data.Ord

lcs :: String -> String -> String
lcs s1 s2 = reverse $ table ! (m, n)
    where
        m = length s1
        n = length s2

        a1 :: Array Int Char
        a1 = listArray (1, m) s1
        
        a2 :: Array Int Char
        a2 = listArray (1, n) s2

        table :: Array (Int, Int) String
        table = listArray ((0, 0), (m, n)) [f i j | i <- [0..m], j <- [0..n]]

        f i j | i == 0 || j == 0     = ""
              | (a1 ! i) == (a2 ! j) = (a1 ! i):(table ! (i-1, j-1))
              | otherwise            = maximumBy (comparing length) [table ! (i-1, j), table ! (i, j-1)]
