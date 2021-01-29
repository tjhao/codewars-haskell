module Codewars.Kata.RemovNB where
      
removNb :: Integer -> [(Integer, Integer)]
removNb n = [(a, round $ getB n a) | a <- [1..n], (&&) <$> isInt <*> (flip elem [1..n] . round) $ getB n a]

isInt x = x == fromInteger (round x)

getB n a = ((fromInteger (n * (n + 1) + 2)) / (fromInteger (2 * (a + 1))) - 1)
