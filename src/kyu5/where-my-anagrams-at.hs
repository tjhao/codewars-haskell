module Anagram where

import Data.List

anagrams :: String -> [String] -> [String]
anagrams w = filter ((==sort w) . sort)
