module Codewars.G964.WeightSort where

import Data.List

orderWeight :: [Char] -> [Char]
orderWeight = unwords . sortBy (\a b -> if trans a /= trans b then compare (trans a) (trans b) else compare a b) . words
                  where trans = sum . map (\x -> read [x] :: Int)
