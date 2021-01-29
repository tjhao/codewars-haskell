module Haskell.Codewars.LookAndSayAndSum where

import Data.List
import Data.Char

lookAndSaySum :: Int -> Int
lookAndSaySum = sum . map digitToInt . show . lns

lns 1 = 1
lns n = trans $ lns (n - 1)
    where trans = read . concatMap ((++) <$> (show . length) <*> ((:[]) . head)) . group . show
