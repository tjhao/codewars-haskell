import Prelude hiding (foldl, reverse)

foldl f z l = foldr trans id l z
  where
    trans x g = \a -> g (f a x)
