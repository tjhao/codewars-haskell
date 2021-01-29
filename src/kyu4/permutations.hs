module Codewars.Kata.Permutations (permutations) where

permutations :: String -> [String]
permutations = remove . per
    where remove [] = []
          remove (x:xs) = if x `elem` xs then remove xs else x : remove xs
          
per :: String -> [String]
per xs = map (map fst) $ myper (zip xs [1..]) (length xs)
    where myper str 0 = [[]]
          myper str n = [p:ps | ps <- myper str (n - 1) , p <- str, p `notElem` ps]
