module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ \inp -> case unP p inp of [] -> []
                                         [(str, v)] -> [(str, f v)]

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) x p = P $ \inp -> case unP p inp of [] -> []
                                         [(str, _)] -> [(str, x)]

infixl 4 <#>
infixl 4 <#

item :: Parser Char
item = P $ \inp -> case inp of [] -> []
                               (c:cs) -> [(cs, c)]

return' :: a -> Parser a
return' x = P $ \inp -> [(inp, x)]

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \inp -> case unP item inp of [] -> []
                                           [(str, c)] -> if p c then [(str, c)] else []
  
-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = predP (== c)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \inp -> [(inp, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P $ \inp -> case unP pf inp of [] -> []
                                           [(str, f)] -> unP (pmap f px) str

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = P $ \inp -> case unP pa inp of [] -> []
                                          [(str, v)] -> case unP pb str of [] -> []
                                                                           [(out, _)] -> [(out, v)]

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = P $ \inp -> case unP pa inp of [] -> []
                                          [(str, _)] -> unP pb str               

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP [] = P $ \inp -> case inp of [] -> [("", "")]
                                     otherwise -> []
stringP s = string' s 

string' :: String -> Parser String
string' [] = return' []
string' (c:cs) = P $ \inp -> case unP (charP c) inp of [] -> []
                                                       [(str, v)] -> case unP (string' cs) str of [] -> []
                                                                                                  [(out, vs)] -> [(out, v:vs)]

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ \_ -> []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
p <<>> q = P $ \inp -> unP p inp ++ unP q inp

infixl 3 <<>>

oneOf :: Parser a -> Parser a -> Parser a
oneOf p q = P $ \inp -> case unP p inp of [] -> unP q inp
                                          [(v, out)] -> [(v, out)]

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = oneOf (some p) (return' [])

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = P $ \inp -> case unP p inp of [] -> []
                                       [(str, v)] ->  case unP (many p) str of [] -> []
                                                                               [(out, vs)] -> [(out, v:vs)]

-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = map snd $ unP p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case filter ((=="") . fst) $ unP p cs of [(_, v)] -> Just v
                                                                otherwise -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr ZeroE = 0
evalExpr (ConstE n) = n
evalExpr (NegE n) = negate $ evalExpr n
evalExpr (BinOpE AddBO e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (BinOpE MulBO e1 e2) = evalExpr e1 * evalExpr e2

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 

digit :: Parser Char
digit = predP isDigit

nat :: Parser Int
nat = P $ \inp -> case unP (some digit) inp of [] -> []
                                               [(out, xs)] -> [(out, read xs)]

zeroE :: Parser Expr
zeroE = (<#) ZeroE (charP 'z')

constE :: Parser Expr
constE = pmap ConstE nat

negE :: Parser Expr
negE = (charP '-') @> pmap NegE expr

add :: Parser Expr
add = BinOpE AddBO <#> (charP '(' @> expr <@ stringP " + ") <@> (expr <@ charP ')')

mult :: Parser Expr
mult = BinOpE MulBO <#> (charP '(' @> expr <@ stringP " * ") <@> (expr <@ charP ')')

binOpE :: Parser Expr
binOpE = add <<>> mult

expr :: Parser Expr
expr = zeroE <<>> constE <<>> negE <<>> binOpE

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique expr
