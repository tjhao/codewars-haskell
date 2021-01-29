module HaskellGolf where

f 1=9
f n=f(n-1)+toInteger(n*9*(10^(n-1)))
