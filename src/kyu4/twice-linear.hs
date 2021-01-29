module Codewars.G964.DblLinear where 

dblLinear :: Int -> Integer
dblLinear n = dbs !! n
    
dbs :: [Integer]
dbs = 1:(merge <$> map ((+1).(2*)) <*> map ((+1).(3*)) $ dbs)
    
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x > y     = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)
