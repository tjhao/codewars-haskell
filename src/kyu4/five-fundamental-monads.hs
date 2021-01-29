{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return x = State (\s -> (x, s))
  (State g) >>= f = State (\s -> let (a, newState) = g s; (State t) = f a in t newState)

instance Monad (Reader s) where
  return x = Reader (\_ -> x)
  (Reader g) >>= f = Reader (\x -> runReader (f (g x)) x)

instance Monoid w => Monad (Writer w) where
  return x = Writer (mempty, x) 
  (Writer (s, v)) >>= f = let (Writer (t, u)) = f v in Writer (s `mappend` t, u)
