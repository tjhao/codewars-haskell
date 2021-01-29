module Josephus where

josephus :: (Ord a) => [a] -> Int -> [a]
josephus xs k = func xs (length xs) k 0

func :: (Ord a) => [a] -> Int -> Int -> Int -> [a]
func _ 0 _ _ = []
func xs n k i = (xs!!newI):func (deleteAt newI xs) (n-1) k newI where newI = (i+k-1) `mod` n

deleteAt :: (Ord a) => Int -> [a] -> [a]
deleteAt n = map snd . filter (\(i, _) -> i /= n) . zip [0..]
