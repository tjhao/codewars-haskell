module LookAndSay where

import Data.List

lookSay :: Integer -> Integer
lookSay = read . concatMap ((++) <$> (show . length) <*> ((:[]) . head)) . group . show
