{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Kata.AdditionCommutes
  ( plusCommutes ) where

import Kata.AdditionCommutes.Definitions
  ( Z, S
  , Natural(..), Equal(..)
  , (:+:))

reflexive :: Natural a -> Equal a a
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS $ reflexive n 

transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS e1) (EqlS e2) = EqlS $ transitive e1 e2

plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes n NumZ = plus0 n
plusCommutes n (NumS m) = transitive (plusSm n m) (EqlS $ plusCommutes n m)

plus0 :: Natural a -> Equal (a :+: Z) a
plus0 NumZ = EqlZ
plus0 (NumS n) = EqlS $ plus0 n

plusSm :: Natural a -> Natural b -> Equal (a :+: S b) (S (a :+: b))
plusSm NumZ m = EqlS $ reflexive m
plusSm (NumS n) m = EqlS $ plusSm n m
