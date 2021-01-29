module Kata (possibilities) where

possibilities :: String -> [String]
possibilities = traverse (\x -> if x `elem` "10" then [x] else "10")
