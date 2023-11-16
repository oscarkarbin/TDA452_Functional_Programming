-- 2A (1) Lists Recursive Data Types

-- LIST RECAP
{-
Can represent several things.

All the things have the same type. Example all bools etc

Ther order matters 1,2,3 /= 3,2,1

Syntax
    - 5 : (6 : (3 : [])) == 5 : 6 : 3 : [] == [5,6,3]

A string is a list of chars
    - "apa" == [a,p,a] ==> type String = [Char]  (type alias!!!)
-}

-- MORE ON TYPES
{-
Functions can have general types
    - Polymorphism (generic types)
    - reverse :: [a] -> [a]         (This is polymorphic functions)
    - (++) :: [a] -> [a] -> [a]        (Join two lists)

Sometimes theses types can be restricted
    - Ord a => for comparisions (>,< ,>= etc)
    - Eq a => for equality
    - Num a => for numeric operations +-* etc

    ex:

    sort does not have :: [a] -> [a] because 
-}


--2A (2) Append and Reverse
import Test.QuickCheck

-- Data.List a standard list definitions
import Data.List(sort)

-- We will hide these functions from the prelude in order to redefine them ourselves.
import Prelude hiding ((++), reverse, drop, take)

import qualified Prelude as P((++), reverse, drop, take)
-- We can now use the standard function with: P.++, P.reverse etc

--append (++)
(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x:(xs ++ ys) 

--reverse. This is a bad revers it has big O n^2
reverse []     = [] 
reverse (x:xs) = reverse xs ++ [x]


--This is better with only big O of n.
rev xs = revInto [] xs
    where revInto acc []     = acc 
          revInto acc (x:xs) = revInto (x:acc) xs

-- 2A (3) Recursio in two parameters take and drop
-- take, drop
-- drop, take :: Int -> [a] -> [a]

take n _ | n <= 0 = []
take _ []         = []
take n (x:xs)     = x : take (n-1) xs



drop n xs | n <= 0 = xs
drop _ []          = []
drop n (x:xs)      = drop (n-1) xs

--2A (4) QuickCheck propereties and tips

--verboseCheck is a command in quickCheck that prints every test made!!!!

--simple property of take (==>)

prop_take :: Int -> [Int] -> Bool
prop_take n xs = 
        let n' = abs n in length (take n' xs) <= n'


--relating drop and take       
prop_takeDrop :: Int -> [Int] -> Bool
prop_takeDrop n xs = take n xs ++ drop n xs == xs