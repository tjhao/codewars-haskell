module Codewars.Kata.Reduction where
import Codewars.Kata.Reduction.Direction

-- data Direction = North | East | West | South deriving (Eq)

dirReduce :: [Direction] -> [Direction]
dirReduce = reverse . foldl foldFunc []
    where foldFunc [] y = y:[]
          foldFunc (x:xs) y = (func x y) ++ xs
    
func North South = []
func South North = []
func East West = []
func West East = []
func x y = y:[x]
