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

data Expr 
    = Num Integer
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Eq, Show)

--expr, term, factor :: Parser Expr
