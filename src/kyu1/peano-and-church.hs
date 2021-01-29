{-# LANGUAGE 
  FlexibleInstances, 
  UndecidableInstances, 
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module PC where

import Data.List

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (f, g)
  where f foo x y = ab $ foo (ba x) (ba y)
        g foo x y = ba $ foo (ab x) (ab y)

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
  zero = O
  successor = S
    
  nat v _ O = v
  nat _ f (S n) = f n

  iter v _ O = v
  iter v f (S n) = iter (f v) f n
    
  plus n O = n
  plus n (S m) = S (plus n m)

  minus O _ = O
  minus n O = n
  minus (S n) (S m) = minus n m
    
  mult O _ = O
  mult (S O) m = m
  mult (S n) m = plus m (mult n m)
    
  pow m O = S O
  pow m (S n) = mult m (pow m n)

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (), 
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
  zero = []
  successor = (():)

  nat v _ [] = v
  nat _ f (_:xs) = f xs

  iter v _ [] = v
  iter v f (_:xs) = iter (f v) f xs
    
  plus n [] = n
  plus n (_:xs) = ():(plus n xs)

  minus [] _ = []
  minus n [] = n
  minus (_:xs) (_:ys) = minus xs ys
    
  mult [] _ = []
  mult [()] m = m
  mult (_:xs) m = plus m (mult xs m)
    
  pow m [] = [()]
  pow m (_:xs) = mult m (pow m xs)

-- Instead of defining Nat from zero, successor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow

  zero = Scott const
  successor n = Scott $ \_ f -> f n
  nat v f n = runScott n v f
  iter v f n = runScott n v (iter (f v) f)

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
-- define predecessor
pair a b = \p -> p a b
true = const
false = flip const
succ' n = \f x -> f (n f x)
phi = \p z -> z (succ' $ p true) (p true)
pred' n = n phi (pair false false) false

instance Nat Church where
  -- Try to implement the calculation (except minus) in the primitive way.
  -- Implement them by constructing Church explicitly.
  -- So plus should not use successor,
  -- mult should not use plus,
  -- exp should not use mult.
  zero = Church $ flip const
  successor (Church n) = Church $ \f x -> f (n f x)
  
  nat v f n | n == zero = v
            | otherwise = f (Church $ pred' $ runChurch n)
  iter v f (Church n) = n f v
  
  plus (Church n) (Church m) = Church $ \f -> (n f) . (m f)
  minus = substR (liftISO2 isoP) minus
  mult (Church n) (Church m) = Church $ n . m
  pow (Church n) (Church m) = Church $ m n
