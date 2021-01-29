{-# LANGUAGE RankNTypes #-}

module Kata where

import Data.Void

type AxiomPEM = forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> b
type AxiomDNE = forall a. ((a -> Void) -> Void) -> a

-- type Formula1 = forall a. forall b. ((a -> Void) -> b) -> (b ->Void) -> (a -> Void) -> Void
-- type Formula2 = forall a. forall b. (a -> b) -> ((a -> Void) -> b) -> (b -> Void) -> Void

-- lemma1 :: Formula1
-- lemma1 na2b nb na = nb $ na2b na

-- lemma2 :: AxiomDNE -> Formula2
-- lemma2 dne a2b na2b nb = nb $ a2b $ dne $ lemma1 na2b nb

-- from :: AxiomDNE -> AxiomPEM
-- from dne a2b na2b = dne $ lemma2 dne a2b na2b

-- one-line version
from :: AxiomDNE -> AxiomPEM
from dne a2b na2b = dne $ \nb -> nb $ a2b $ dne $ (\na2b nb na -> nb $ na2b na) na2b nb

to :: AxiomPEM -> AxiomDNE
to pem nna = pem id (absurd . nna)
