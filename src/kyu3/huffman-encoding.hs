module Huffman
    ( frequencies
    , encode
    , decode
    , Bit (..)
    ) where

import Data.List

data Bit = Z | O deriving (Eq, Show)

data Tree a = Leaf a Int | Node Int (Tree a) (Tree a)

type HCode = [Bit]

type Table a = [(a, HCode)]

-- | Calculate symbol frequencies of a text.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = sortBy (\(_, a) (_, b) -> compare a b) . map (\x -> (head x, length x)) . group . sort

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe HCode
encode [] _ = Nothing
encode [f] _ = Nothing
encode fs chs = Just $ encodeMessage (makeTable $ makeTree fs) chs

-- | encodeMessage
encodeMessage :: Ord a => Table a -> [a] -> HCode
encodeMessage table = concat . map (lookupTable table)

lookupTable :: Ord a => Table a -> a -> HCode
lookupTable [] _ = error "error"
lookupTable ((ch, h):xs) c
    | ch == c   = h
    | otherwise = lookupTable xs c

-- | makeTree
makeTree :: Ord a => [(a, Int)] -> Tree a
makeTree = makeCodes . toTreeList

toTreeList :: Ord a => [(a, Int)] -> [Tree a]
toTreeList = map (uncurry Leaf)

makeCodes :: Ord a => [Tree a] -> Tree a
makeCodes [t] = t 
makeCodes ts = makeCodes (amalgamate ts)

amalgamate :: Ord a => [Tree a] -> [Tree a]
amalgamate [] = []
amalgamate [t] = [t]
amalgamate (t1:t2:ts) = insTree (pair t1 t2) ts

value :: Ord a => Tree a -> Int
value (Leaf _ n) = n
value (Node n _ _) = n

insTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insTree t [] = [t]
insTree t all@(th:ts)
    | (value t) > (value th) = th : insTree t ts
    | otherwise = t:all

pair :: Ord a => Tree a -> Tree a -> Tree a
pair t1 t2 = Node ((value t1) + (value t2)) t1 t2

-- | makeTable
convert :: Ord a => HCode -> Tree a -> Table a
convert hs (Leaf c _) = [(c, hs)]
convert hs (Node _ left right) = (convert (hs ++ [Z]) left) ++ (convert (hs ++ [O]) right)

makeTable :: Ord a =>  Tree a -> Table a
makeTable = convert []

-- | Decode a bit sequence using the given frequencies.
decode :: Ord a => [(a, Int)] -> HCode -> Maybe [a]
decode [] _ = Nothing
decode [f] _ = Nothing
decode fs chs = Just $ decodeMessage (makeTree fs) chs

-- | decodeMessage
decodeMessage :: Ord a => Tree a -> HCode -> [a]
decodeMessage tr = decodeByt tr
    where decodeByt (Node n left right) (Z:hs) = decodeByt left hs
          decodeByt (Node n left right) (O:hs) = decodeByt right hs
          decodeByt (Leaf ch n) hs = ch : decodeByt tr hs
          decodeByt t [] = []
