module LargestSum where 

largestSum :: [Int] -> Int
largestSum = maximum . scanl (\acc x -> max 0 acc + x) 0
