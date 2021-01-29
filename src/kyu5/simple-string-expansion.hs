module StringExpansion where 

import Data.Char
import Data.List

solve :: [Char] -> [Char]
solve = foldr (\x acc -> if all isAlpha x then x ++ acc else take ((read x) * length acc) $ cycle acc) "" . map (filter (\x -> x /= '(' && x /= ')')) . groupBy (\x y -> isDigit x == isDigit y) 
