module Fixit where
import Prelude hiding (reverse, foldr)

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' _ [] = []
reverse' f (x:xs) = f xs ++ [x]

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' _ _ z [] = z
foldr' g f z (x:xs) =  f x (g f z xs)
