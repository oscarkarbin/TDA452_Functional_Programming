import Data.Char
import Data.Maybe
import Test.QuickCheck
import Parsing hiding (chain, digit)

import Control.Monad(forever)


-- 5A (1) Parsing Intro and Overview

--Parsing == transforming a flat string into something with a richer meaning


-- number :: String -> Int ??? 


--5A (2) Basic Parsing from Scratch
-- a parser for an X
-- String -> Maybe (X, String)
--  * if the paraser fail give Nothing
--  * if the parser is good, return Just(x,r). X is result and r is remainder of input.

type ParserFun a = String -> Maybe(a, String)

num :: ParserFun Integer
num s = case span isDigit s of
    (d:ds, rest) -> Just(read (d:ds), rest)
    _            -> Nothing

addition0 :: ParserFun Integer
addition0 s = case num s of 
    Just (n, '+':r) -> case num r of
        Just (m, r') -> Just (n+m, r')
        _            -> Nothing
    _               -> Nothing

multi0 :: ParserFun Integer
multi0 s = case num s of 
    Just (n, '*':r) -> case num r of
        Just (m, r') -> Just (n*m, r')
        _            -> Nothing
    _               -> Nothing

calculation0 s = case addition0 s of
    Nothing -> multi0 s
    ok      -> ok

-- 5A (3) - A Parsing Library
-- rewriting our first warmup example from before

digit :: Parser Char
digit = sat isDigit

number :: Parser Integer
number = read <$> oneOrMore digit

operation c op = do
    n <- number
    sat(== c)
    m <- number
    return(m `op` n)

addition :: Parser Integer
addition = operation '+' (+)

multiplication :: Parser Integer
multiplication = operation '*' (*)

calculation :: Parser Integer
calculation = addition <|> multiplication

-- 5A (4) Expression Parser version 1

eval :: Expr -> Integer
eval (Num n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b


data Expr 
    = Num Integer
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Eq, Show)

expr, term, factor :: Parser Expr

expr = foldl1 Add <$> chain term (char '+')
term = foldl1 Mul <$> chain factor (char '*')
factor = Num <$> number <|> char '(' *> expr <* char ')'


{- -- simplifications
do
char '('
e <- expr
char ')'
return e
-}
-- do
-- t <- term
-- ts <- zeroOrMore (do char '+'; term)
-- return $ foldl Add t ts
-- do
-- ts <- chain factor (char '*')
-- return (foldl Mul ts)
-- t <- factor
-- ts <- zeroOrMore (do char '*'; factor)
-- return $ foldl Mul t ts

-- library function:
chain :: Parser item -> Parser sep -> Parser [item]
chain item sep = do
    i <- item
    is <- zeroOrMore (do sep; item)
    return (i:is)

--------------------------------------------------------------------------------
-- * The simple calculator example
main = do 
    putStrLn "Welcome to the simple calculator!"
    forever readEvalPrint

readEvalPrint = do
    putStr "What would you like to calculate?"
    s <- getLine
    let s' = filter (not . isSpace) s
    case parse expr s' of
        Just (e, "") -> print $ eval e
        Nothing -> putStrLn "Invalid Expression!"
