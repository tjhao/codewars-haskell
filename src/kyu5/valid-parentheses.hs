module Codewars.Parentheses where

validParentheses :: String -> Bool
validParentheses = null . foldr (\x acc -> if x == ')' then x:acc else (if not $ null acc then (if head acc == ')' then tail acc else 'f':acc) else 'f':acc)) []
