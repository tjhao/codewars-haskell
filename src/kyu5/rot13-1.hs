module Rot13 where

import Data.Char

rot13 :: String -> String
rot13 = map (\c -> chr . trans $ ord c)

trans n
    | n >= 65 && n <= 90  = 65 + (n - 65 + 13) `mod` 26
    | n >= 97 && n <= 122 = 97 + (n - 97 + 13) `mod` 26
    | otherwise           = n
