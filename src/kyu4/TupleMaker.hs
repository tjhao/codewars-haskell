{-# LANGUAGE TemplateHaskell #-}
module TupleMaker (tuple) where

import Language.Haskell.TH
import Control.Monad

-- | Creates a lambda that takes `n` arguments and
-- | returns an n-tuple of those arguments.
tuple :: Int -> Q Exp
tuple 0 = [| () |]
tuple 1 = [| id  |]
tuple n = do
  xs <- replicateM n (newName "x")
  -- let xs = map (\x -> mkName $ "x" ++ show x) [1..n]
  return $ LamE (map VarP xs) (TupE $ map VarE xs)
