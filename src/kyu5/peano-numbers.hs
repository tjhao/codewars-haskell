module Haskell.Codewars.Peano where
import Prelude hiding (even, odd, div, compare, Num, Int, Integer, Float, Double, Rational, Word)

data Peano = Zero | Succ Peano deriving (Eq, Show)

add, sub, mul, div :: Peano -> Peano -> Peano
-- Addition
add m Zero = m
add m (Succ n) = Succ (add m n)
-- Subtraction
sub m Zero = m
sub Zero m = error "negative number"
sub (Succ m) (Succ n) = sub m n
-- Multiplication
mul m Zero = Zero
mul m (Succ n) = add m $ mul m n 
-- Integer division
div _ Zero = error "divide by 0"
div m n = div' m n Zero 
    where div' m n c = if compare m n == LT then c else div' (sub m n) n (Succ c)

even, odd :: Peano -> Bool
-- Even
even Zero = True
even (Succ m) = not $ even m  
-- Odd
odd = not . even

compare :: Peano -> Peano -> Ordering
-- Compare
compare Zero Zero = EQ 
compare m Zero = GT
compare Zero m = LT 
compare (Succ m) (Succ n) = compare m n
