module Calculator where

import Control.Applicative hiding (many)
import Control.Monad (liftM, ap)
import Data.Char

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where   
    return v = Parser $ \inp -> [(v, inp)]
    (Parser p) >>= f = Parser $ \inp -> case p inp of 
                                             [] -> []
                                             [(v, out)] -> let (Parser q) = f v in q out

item :: Parser Char
item = Parser $ \inp -> case inp of 
                            [] -> []
                            cs -> [(last cs, init cs)] 

failure :: Parser a
failure = Parser $ \_ -> []

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat $ (||) <$> isDigit <*> (=='.')

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return $ reverse (x:xs)

choose :: Parser a -> Parser a -> Parser a
choose p q = Parser $ \inp -> case parse p inp of [] -> parse q inp
                                                  [(v, out)] -> [(v, out)]
oneOf3 p q r = p `choose` q `choose` r 

many :: Parser a -> Parser [a]
many p = many1 p `choose` return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

dou :: Parser Double
dou = do xs <- many1 (digit)
         return $ read $ reverse xs

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

double :: Parser Double
double = token dou

symbol :: String -> Parser String
symbol xs = token (string xs)

expr :: Parser Double
expr = do t <- term
          oneOf3 (do symbol "+"; e <- expr; return $ t + e) 
                 (do symbol "-"; e <- expr; return $ e - t) 
                 (return t)
     
term :: Parser Double
term = do f <- factor
          oneOf3 (do symbol "*"; t <- term; return $ f * t) 
                 (do symbol "/"; t <- term; return $ t / f) 
                 (return f)

factor :: Parser Double
factor = (do symbol "("
             e <- expr
             symbol ")"
             return e) `choose` double

evaluate :: String -> Double
evaluate xs = case parse expr xs of 
              [(n, [])] -> n
              [(_, out)] -> error ("unused input " ++ out)
              [] -> error "invalid input"
