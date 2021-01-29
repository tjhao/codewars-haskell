{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))


-- SPair
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a, b)
toPair (SPair p) = p (,)
-- toPair = (,) <$> fst <*> snd

fromPair :: (a, b) -> SPair a b
fromPair (a, b) = SPair (\f -> f a b)

fst :: SPair a b -> a
fst (SPair p) = p const

snd :: SPair a b -> b
snd (SPair p) = p $ (\_ b -> b)

swap :: SPair a b -> SPair b a
swap p = fromPair $ (snd p, fst p)

curry :: (SPair a b -> c) -> (a -> b -> c)
curry f a b = f $ fromPair (a, b)

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = f <$> fst <*> snd 


-- SMaybe
newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe m) = m Nothing Just

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing = SMaybe const
fromMaybe (Just x) = SMaybe (\_ f -> f x)

isJust :: SMaybe a -> Bool
isJust (SMaybe m) = m False (\_ -> True)

isNothing :: SMaybe a -> Bool
isNothing (SMaybe m) = m True (\_ -> False)


-- SEither
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }

toEither :: SEither a b -> Either a b
toEither (SEither e) = e Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left x) = SEither (\l _ -> l x)
fromEither (Right x) = SEither (\_ r -> r x)

isLeft :: SEither a b -> Bool
isLeft (SEither e) = e (\_ -> True) (\_ -> False)

isRight :: SEither a b -> Bool
isRight (SEither e) = e (\_ -> False) (\_ -> True)


-- SList
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }

toList :: SList a -> [a]
toList (SList l) = l [] (\x xs -> x:toList xs)

fromList :: [a] -> SList a
fromList [] = SList const
fromList (x:xs) = SList (\_ f -> f x (fromList xs))

nilS :: SList a
nilS = SList const

cons :: a -> SList a -> SList a
cons x xs =  SList (\_ f -> f x xs)

add :: a -> SList a -> SList a
add a (SList l) = l (cons a nilS) (\x xs -> cons x (add a xs))

concat :: SList a -> SList a -> SList a
concat l (SList l') = l' l (\x xs -> concat (add x l) xs) 

null :: SList a -> Bool
null (SList l) = l True (\_ _ -> False)

length :: SList a -> Int
length (SList l) = l 0 (\_ xs -> 1 + length xs) 

map :: (a -> b) -> SList a -> SList b
map f (SList l) = l nilS (\x xs -> cons (f x) (map f xs))

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f b (SList l) = l b (\x xs -> foldl f (f b x) xs)

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f b (SList l) = l b (\x xs -> f x (foldr f b xs))

take :: Int -> SList a -> SList a
take n (SList l) | n <= 0    = nilS
                 | otherwise = l nilS (\x xs -> cons x (take (n-1) xs))


zip :: SList a -> SList b -> SList (SPair a b)
zip l1 l2 = case (toList l1, toList l2) of 
                (x:xs, y:ys) -> cons (fromPair (x, y)) (zip (fromList xs) (fromList ys))
                _ -> nilS

catMaybes :: SList (SMaybe a) -> SList a
catMaybes = foldr f nilS where f x acc = case toMaybe x of 
                                             Nothing -> acc
                                             Just v -> cons v acc

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = fromPair . foldr f (nilS, nilS) 
                where f x (as, bs) = case toEither x of Left a -> (cons a as, bs)
                                                        Right b -> (as, cons b bs)
