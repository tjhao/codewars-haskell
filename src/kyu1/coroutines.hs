module Coroutine where

import Control.Monad (ap, forever)
import Preloaded

-- Preloaded contains the following:
-- {-# LANGUAGE DeriveFunctor #-}
--
-- newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)
--
-- data Command r u d a =
--   Done a
-- | Out d (Coroutine r u d a)
-- | In (u -> Coroutine r u d a) deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
  return x = Coroutine $ \k -> k $ Done x
  f >>= g  = Coroutine $ \k -> apply f $
    \x -> case x of Done a -> apply (g a) k
                    Out d f1 -> k $ Out d $ f1 >>= g
                    In h -> k $ In $ \u -> (h u) >>= g

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
p1 >>> p2 = Coroutine $ \k -> apply p2 $ 
  \x -> case x of Done a -> k $ Done a
                  Out d p2' -> k $ Out d $ p1 >>> p2'
                  In h -> apply p1 $
                          \x -> case x of Done a -> k $ Done a
                                          Out m p1' -> apply (p1' >>> h m) k
                                          In g -> k $ In $ \u -> (g u >>> p2)

-- It might be useful to define the following function
-- pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a

-- Library functions

output :: a -> Coroutine r u a ()
output v = Coroutine $ \k -> k $ Out v $ Coroutine $ \k -> k $ Done ()

input :: Coroutine r v d v
input = Coroutine $ \k -> k $ In $ \v -> Coroutine $ \k -> k $ Done v

produce :: [a] -> Coroutine r u a ()
produce [] = Coroutine $ \k -> k $ Done ()
produce (x:xs) = Coroutine $ \k -> k $ Out x $ produce xs

consume :: Coroutine [t] u t a -> [t]
consume c = apply c $
  \x -> case x of Done _ -> []
                  Out t c1 -> t:consume c1

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = Coroutine $ \k -> k $ In $ 
  \v -> if p v then Coroutine $ \k -> k $ Out v $ filterC p else filterC p

mapC :: (v -> u) -> Coroutine r v u ()
mapC f = Coroutine $ \k -> k $ In $
  \v -> Coroutine $ \k -> k $ Out (f v) $ mapC f

limit :: Int -> Coroutine r v v ()
limit n 
  | n <= 0    = Coroutine $ \k -> k $ Done ()
  | otherwise = Coroutine $ \k -> k $ In $ \v -> Coroutine $ \k -> k $ Out v $ limit (n-1)

suppress :: Int -> Coroutine r v v ()
suppress 0 = Coroutine $ \k -> k $ In $ \v -> Coroutine $ \k -> k $ Out v $ suppress 0
suppress n = Coroutine $ \k -> k $ In $ \_ -> suppress (n-1)

add :: Coroutine r Int Int ()
add = Coroutine $ \k -> k $ In $ \n -> Coroutine $ \k -> k $ In $ \m -> Coroutine $ \k -> k $ Out (n+m) $ add

duplicate :: Coroutine r v v ()
duplicate = Coroutine $ \k -> k $ In $ \n -> Coroutine $ \k -> k $ Out n $ Coroutine $ \k -> k $ Out n $ duplicate

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers 
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filterC even >>> limit 5
p2 = produce [1..] >>> mapC (\n -> n*(n+1) `div` 2)
p3 = mapC (*2)
p4 = duplicate >>> suppress 1 >>> add
