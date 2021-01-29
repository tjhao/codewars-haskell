module Codewars.Kata.Hashtag where

import Data.List
import Data.Char

generateHashtag :: String -> Maybe String
generateHashtag xs
    | length xs > 140 = Nothing
    | null $ words xs = Nothing
    | otherwise       = Just . ('#':) . concatMap ((:) <$> (toUpper . head) <*> tail) . words $ xs
