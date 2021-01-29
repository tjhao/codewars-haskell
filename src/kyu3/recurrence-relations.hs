-- https://stackoverflow.com/questions/141650/how-do-you-make-a-generic-memoize-function-in-haskell
module FunctionEvaluator where

import Data.Either
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe
import Data.Function (fix)

memoize :: Ord a => (a -> b) -> (a -> b)
memoize f = unsafePerformIO $ do 
    r <- newIORef Map.empty
    return $ \x -> unsafePerformIO $ do 
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do 
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y
                    
transferFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> (a -> b) -> (a -> b)
transferFunction origin_f f x
    | isLeft $ origin_f x = fromLeft1 $ origin_f x
    | otherwise = (\(a, g) -> g (map f a)) $ fromRight1 $ origin_f x

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f = fix (memoize . (transferFunction f))

fromLeft1 :: Either a b -> a
fromLeft1 (Left x) = x

fromRight1 :: Either a b -> b
fromRight1 (Right x) = x