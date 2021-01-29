import Data.Array

snail :: [[Int]] -> [Int]
snail [[]]  = []
snail input = foo 1 n []
  where
    n = length input

    table :: Array (Int, Int) Int
    table = listArray ((1, 1), (n, n)) $ concat input

    loop k n 
      | k == n    = [(n, n)]
      | otherwise = (map ((,) k) [k..n]) ++ (map (flip (,) n) [k+1..n-1]) ++ (map ((,) n) $ reverse [k..n]) ++ (map (flip (,) k) $ reverse [k+1..n-1])

    foo k n l
      | k > n     = l
      | otherwise = foo (k+1) (n-1) $ l ++ (map (table !) $ loop k n)