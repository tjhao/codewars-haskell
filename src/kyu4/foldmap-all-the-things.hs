-- GHC 7.10.3
module Foldmap where

import Data.Foldable (foldMap, Foldable)
import Data.Monoid
import Data.Semigroup

myToList :: Foldable t => t a -> [a]
myToList = foldMap (:[])

data MyMaybe a = MyNothing | MyJust a

instance Ord a => Monoid (MyMaybe a) where
  mempty = MyNothing
  mappend = (Data.Semigroup.<>)

instance Ord a => Semigroup (MyMaybe a) where
  MyNothing <> (MyJust v) = MyJust v
  (MyJust v) <> MyNothing = MyJust v
  (MyJust v) <> (MyJust u) = if u < v then MyJust u else MyJust v

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum = trans . foldMap MyJust
  where
    trans MyNothing  = Nothing
    trans (MyJust v) = Just v

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f init t = appEndo (foldMap (\a -> Endo (f a)) t) init
