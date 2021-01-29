{-# LANGUAGE GADTs #-}
module Combinators where
import PredefinedCombinators (SKI(..))

evalSKI :: SKI a -> a
evalSKI I = id
evalSKI K = const
evalSKI S = \x y z -> x z (y z) 
evalSKI (Ap a b) = (evalSKI a) (evalSKI b)

prettyPrintSKI :: SKI a -> String
prettyPrintSKI I = "I"
prettyPrintSKI K = "K"
prettyPrintSKI S = "S"
prettyPrintSKI (Ap a b) = "(" ++ prettyPrintSKI a ++ " " ++ prettyPrintSKI b ++ ")"

(!) :: SKI (a -> b) -> SKI a -> SKI b
(!) = Ap
infixl 5 !

-- helper: combinator B and C
b = S!(K!S)!K
c = S!(b!b!S)!(K!K)


comp :: SKI ((b -> c) -> (a -> b) -> (a -> c))
comp = b

rev :: SKI (a -> (a -> b) -> b)
rev = b!(S!I)!K

flip' :: SKI ((a -> b -> c) -> (b -> a -> c))
flip' = c

rotr :: SKI (a -> (c -> a -> b) -> c -> b)
rotr = b!(S!S)!(b!K!K)

rotv :: SKI (a -> b -> (a -> b -> c) -> c)
rotv = c!(b!S!(b!K!(b!S!(b!(S!I)!K))))!K

join :: SKI ((a -> a -> b) -> a -> b)
join = c!(b!S!(c!(b!S!K)!I))!I

type Bool' a = a -> a -> a

true = K

false = Ap K I 

not' = c!(c!I!(K!I))!K

and' = c!S!(K!false)

or' = c!(b!S!(c!(b!S!K)!(K!true)))!I

xor' = b!((c!I)!I)!((c!I)!not')
