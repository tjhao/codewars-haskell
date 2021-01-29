module Sudoku where

import Data.List.Split
import Data.List

type Row a = [a]
type Matrix a = [Row a]
type Grid = Matrix Int

nums :: [Int]
nums = [1..9]

blank :: Int -> Bool
blank = (==0)

choices :: Grid -> Matrix [Int]
choices = map (map choice) where choice d = if blank d then nums else [d]

-- rows, cols and boxes
rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxes :: Matrix a -> Matrix a
boxes = map concat . concat . map cols . split3 . map split3 where split3 = splitEvery 3

-- prune
prune :: Matrix [Int]-> Matrix [Int]
prune = pruneBy boxes . pruneBy cols . pruneBy rows
    where pruneBy f = f . map pruneRow . f

pruneRow :: Row [Int] -> Row [Int]
pruneRow row = map (remove fixed) row 
    where fixed = [x | [x] <- row] 

remove :: [Int] -> [Int] -> [Int]
remove _ [x] = [x]
remove fs xs = [x | x <- xs, x `notElem` fs]

-- solve
single :: [a] -> Bool
single [_] = True
single _ = False

complete :: Matrix [Int] -> Bool
complete = all (all single)

expand :: Matrix [Int] -> [Matrix [Int]]
expand rows = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
    where (rows1, row:rows2) = span (not . any smallest) rows
          (row1, cs:row2)    = span (not . smallest) row
          smallest cs        = length cs == n
          n                  = minimum $ counts rows
          counts             = filter (/= 1) . map length . concat

safe :: Matrix [Int] -> Bool
safe m = all ok (rows m) && all ok (cols m) && all ok (boxes m)
         where ok row = nodups [x | [x] <- row]

extract :: Matrix [Int] -> Grid
extract = map (map head)

nodups :: (Eq a) => [a] -> Bool
nodups xs = length xs == (length $ nub xs)

search :: Matrix [Int] -> [Grid]
search cm
    | not (safe pm) = []
    | complete pm = [extract pm]
    | otherwise = concatMap search (expand pm)
        where pm = prune cm

sudoku :: Grid -> Grid
sudoku = head . search . choices
