{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

module Kata.TimesComm where

import Kata.TimesComm.Definitions

{- Preloaded code. Maybe helpful for local editing.

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | Peano definition of multiplication.
type family (:*:) (n :: *) (m :: *) :: *
type instance Z :*: m = Z
type instance S n :*: m = m :+: (n :*: m)

-}

reflexive :: Natural a -> Equal a a
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS $ reflexive n 

symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS e) = EqlS $ symmetric e

transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS e1) (EqlS e2) = EqlS $ transitive e1 e2

-- data-level addition
add :: Natural n -> Natural m -> Natural (n :+: m)
NumZ `add` m = m
(NumS n) `add` m = NumS $ n `add` m

-- data-level multiplication
mult :: Natural n -> Natural m -> Natural (n :*: m)
NumZ `mult` m = NumZ
(NumS n) `mult` m = m `add` (n `mult` m)

-- This will be helpful
plus' :: Equal n m -> Natural a -> Equal (n :+: a) (m :+: a)
plus' EqlZ p = reflexive p
-- plus' (EqlS e) NumZ = EqlS $ p
plus' (EqlS e) p = EqlS $ plus' e p
-- plus' (EqlS e) (NumS p) = EqlS $ transitive (transitive (plusSm n p) (EqlS $ plus' e p)) (symmetric $ plusSm m p)

-- alter the position
front :: Natural a -> Natural b -> Natural c -> Equal (a :+: c) (b :+: c) -> Equal (c :+: a) (c :+: b)
front m n p e = transitive (transitive e1 e) e2
  where
    e1 = symmetric $ plusComm m p
    e2 = plusComm n p

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-plus-c-equals-a-plus-b-plus-c-prove-it/haskell
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ m p = reflexive $ m `add` p
plusAssoc (NumS n) m p = EqlS $ plusAssoc n m p

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/haskell
plusComm :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusComm n NumZ = plus0 n
plusComm n (NumS m) = transitive (plusSm n m) (EqlS $ plusComm n m)

plus0 :: Natural a -> Equal (a :+: Z) a
plus0 NumZ = EqlZ
plus0 (NumS n) = EqlS $ plus0 n

plusSm :: Natural a -> Natural b -> Equal (a :+: S b) (S (a :+: b))
plusSm NumZ m = EqlS $ reflexive m
plusSm (NumS n) m = EqlS $ plusSm n m

multmSn :: Natural a -> Natural b -> Equal (a :+: (a :*: b)) (a :*: S b)
multmSn NumZ n = EqlZ
multmSn (NumS m) n = EqlS $ transitive (transitive (transitive e1 e2) e3) e4
  where
    e1 = plusAssoc m n (m `mult` n)
    e2 = plus' (plusComm m n) (m `mult` n)
    e3 = symmetric $ plusAssoc n m (m `mult` n)
    e4 = front (m `add` (m `mult` n)) (m `mult` ((NumS n))) n $ plus' (multmSn m n) n

-- This will also be helpful
zeroComm :: Natural a -> Equal Z (a :*: Z)
zeroComm NumZ = EqlZ
zeroComm (NumS n) = zeroComm n

-- -- This is the proof that the kata requires.
-- -- | a * b = b * a
timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm m NumZ = symmetric $ zeroComm m
timesComm m (NumS n) = transitive e1 e2
  where
    e1 = symmetric $ multmSn m n
    e2 = front (m `mult` n) (n `mult` m) m $ plus' (timesComm m n) m
