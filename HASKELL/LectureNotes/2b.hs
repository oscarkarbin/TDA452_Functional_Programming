import Data.Char(isSpace)
import Data.List(sort, group, groupBy)


--2B (0)
--Higher order functions
{-
A function which take another function as a parameter
-}

even1 :: Int -> Bool
even1 n = n `mod` 2 == 0

-- The type of a higher order function is. example:
-- :t map ===> (a->b) -> [a] -> [b]      map is function with a function as a parameter or also called a higher order function

map1 f xs    = [f x | x <- xs]

filter2 p xs = [x | x <- xs, p x]

--2B (1)
-- Combining elements - foldr

sum1 []     = 0
sum1 (n:ns) = n + sum1 ns

and1 [] = True  -- The identity of a bool is True
and1 (b:bs) = b && and1 bs

-- This is generalization of the functions above. A right fold.
-- this is powerful pattern for combining elements in a list. 
-- There also exists a left fold.
foldr' op b [] = b  -- b is the base case.
foldr' op b (x:xs) = x `op` foldr' op b xs

unlines' ss = foldr' joinNl "" ss    -- joinNl is an example on how we feed a higher order function, by simply naming it!!! It can be local.
    where joinNl s1 s2 = s1 ++ "\n" ++ s2

--2B (2) A common pattern - takeWhile
takeLine "" = ""
takeLine (c:cs) | c /= '\n' = c:takeLine cs 
                | otherwise = ""

takeWord ""                       = ""
takeWord (c:cs) | not (isSpace c) = c:takeWord cs 
                | otherwise       = ""

-- This is also a generalization of the two functions above.      
takeWhile' ::  (a -> Bool) -> [a] -> [a]
takeWhile' p []                 = []
takeWhile' p (x:xs) | p x       = x:takeWhile' p xs
                    | otherwise = []

-- 2B (3) Lambda Expressions (anon functions)

takeLine'' cs = takeWhile' (\x -> x /= '\n') cs 

-----------------------------------------------------
-- 2B (4) Sections

-- we can write operators that are wating for the other argunet.
-- ex (+1), (<0)

takeLine''' cs = takeWhile' (/= '\n') cs
---------------------------------------------------------------
--2B (5) Dropwhile examples

lines' [] = []
lines' css = takeWhile (/= '\n') css: lines' (drop 1 (dropWhile (/= '\n') css))

--2B (6) Example - generalising lines - segments

nosh = "spam,eggs,chips,spam"

commSep [] = []
commSep css = takeWhile (/= ',') css: commSep (drop 1 (dropWhile (/= ',') css))

-- Generalising lines
segments p [] = []
segments p xs = takeWhile p xs: segments p (drop 1(dropWhile p xs))

-- SB (7) - Partial Application

-- This is just functions
f :: Char -> Bool -> String -> String
f c b s = c:s ++ show (not b) ++ s

-- SB (8)  Style checking with hlint

-- hlint is a useful tool for spotting code waste
-- It will give stylistic hints
-- hlint is built into haskell

--------------------------------------------------

-- SB (9) Understanding precedence - avoiding brackets

-- we can use $ instead of brackets because it has a low precedent level.
-- space has the highest precedence level

-- SB (10) Example - putting it all together


wordCount s = unlines
            . map (\(w,n) -> w ++ ": " ++ show n)
            . map (\ws -> (head ws, length ws)) 
            . group 
            . sort 
            . words 

--------------------------------------------------