import Data.Array

balancedParens :: Int -> [String]
balancedParens n = map reverse $ table ! (n, n)
  where  
    table :: Array (Int, Int) [String]
    table = listArray ((0, 0), (n, n)) [f i j [""] | i <- [0..n], j <- [0..n]]

    f i j p | i == 0    = map ((take j $ repeat ')') ++ ) p
            | i == j    = f (i-1) j (map ('(':) p) 
            | i < j     = f (i-1) j (map ('(':) p) ++ f i (j-1) (map (')':) p)
            | otherwise = [""]