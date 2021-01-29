{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module PolyvariadicFunctions where

class FromInt r where
  fromInt :: Int -> r

instance FromInt Int where
  fromInt = id

instance (i ~ Int, FromInt r) => FromInt (i -> r) where
  fromInt i j = fromInt $ i + j

polyAdd :: (FromInt r) => r 
polyAdd = fromInt 0


class FromString r where
  fromString :: String -> r

instance FromString String where
  fromString = id

instance (FromString r) => FromString (String -> r) where
  fromString s s' = case s of ""        -> fromString s'
                              otherwise -> fromString $ s ++ " " ++ s'

polyWords :: (FromString r) => r 
polyWords = fromString ""


class FromList a r | r -> a where
  fromList :: [a] -> r

instance FromList a [a] where
  fromList = id

instance (FromList a r) => FromList a (a -> r) where
  fromList xs x = fromList $ xs ++ [x]

polyList :: (FromList a r) => r
polyList = fromList []