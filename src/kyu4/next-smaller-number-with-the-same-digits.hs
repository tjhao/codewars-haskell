module NextSmaller where 

import Data.List
import Data.Ord

check :: String -> [(Bool, Int)]
check xs = filter fst . flip zip [0..] $ zipWith (>) xs (tail xs)

getIndex :: String -> Int
getIndex = snd . last . check

par :: String -> (String, String)
par xs = splitAt (getIndex xs + 1) xs

target :: Char -> String -> (Char, Int)
target c xs = maximum . filter ((<c) . fst) $ zip xs [0..]

change :: String -> String -> (String, String)
change s1 s2 = (newS1, newS2)
    where leftChar = last s1
          right = target leftChar s2
          rightChar = fst right
          rightIndex = snd right
          newS1 = (init s1) ++ [rightChar]
          newS2 = map (\(x, y) -> if y == rightIndex then leftChar else x) $ zip s2 [0..]

final :: String -> String
final s = (\(x, y) -> x ++ (sortOn Down y)) . uncurry change $ par s

nextSmaller :: Integer -> Maybe Integer
nextSmaller n
    | null . check $ sn           = Nothing
    | (=='0') . head . final $ sn = Nothing
    | otherwise                   = Just . read . final $ sn
        where sn = show n
