{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Prelude hiding (filter)
import System.Random(randomRs,mkStdGen)
import GHC.Natural (naturalFromInteger)


{-
1
Give a recursive definition of the function filter.
-}

-- 1

-- not recursive..
filter1 :: (a-> Bool) -> [a] -> [a]
filter1 f xs = [x | x <- xs, f x]

-- Giga recursive :)))
filter2 :: (a-> Bool) -> [a] -> [a]
filter2 f []     = []
filter2 f (x:xs) | not(f x)    = filter2 f xs
                 | f x    = x:filter2 f xs 
 

not2 n | n == 2    = False
       | otherwise = True 

{-
2
(Not Higher-Order) Suppose that some phone companies bill by the second, and some others by 10-second intervals. 
Generalise the function callCost even more, and give a new definition for telefartCost
-}

callCost openingCost perMinuteCost seconds =
     openingCost + fromInteger (seconds `div` 60) * perMinuteCost


callCost1 startCost costPerSecond interval seconds = 
    startCost + (costPerSecond * interval) * (seconds / interval)

telefartCost seconds = callCost1 1.5 (0.5/60) 60 seconds

{-
Generalise the following functions by defining a higher-order function:

    countEven ns     = length [() | n <- ns, even n]
    countNegative ns = length [() | n <- ns, n < 0]
    countFalse xs    = length [() | b <- bs, not b]

and show how each of these can be defined using your more general definition.

Can you also define this one using your higher-order function?

    countTrue xs = length [() | b <- bs, b]

-}


counter :: [a] -> (a -> Bool) -> Int
counter [] f     = 0
counter xs f = length [() | x <- xs, f x]

countEven ns = counter ns even
countNegative ns = counter ns (<0)
countFalse xs = counter xs not

countTrue xs = counter xs (not . not) -- id

{-
4
Define (using recursion) a higher-order function iter such that 
iter n f x 
applies the function f to the argument x n times. 
So for example iter 3 (++"!") "Hi" would give "Hi!!!".
-}

iter 0 f x = x
iter n f x = iter (n-1) f (f x)

{-
5
Define the power function from Lab 1 using iter.
-}

{-
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)
-}

power n k = iter (k-1) (*n) n

{-
Define iter using the standard function iterate.
-}

iter' n f x = head $ drop n $ take (n+1) (iterate f x)

{-
7 🌶️🌶️🌶️🌶️

Recall the function sales::WeekNumber -> Integer described in the exercises from week 1, 
where the sales n is the sales for week number n (a non negative Int). 

A sales streak is a contiguous sequence of week numbers k, k+1, k+2,...k+m  
where the sales strictly increase from one week to the next.  
Here there are m increases in a row, so we say that there is a streak of length m. 

Define a function longestStreak :: WeekNumber -> Int where longestStreak n computes the longest sales streak 
in the range of weeks from 0 to n.

You can attempt to solve this in two different ways 
(i) using recursion  
and 
(ii) by combining standard functions like the ones in the lectures and (perhaps) zipWith

-}

-- (i)
type WeekNumber = Int
sales :: WeekNumber -> Integer
sales i = randomRs (0,1000) (mkStdGen i) !! i

longestStreak :: WeekNumber -> Int
longestStreak n = longestStreakHelper 0 0 n

longestStreakHelper currentStreak maxStreak 0 = maxStreak
longestStreakHelper currentStreak maxStreak n
    | sales n >= sales (n-1) && currentStreak < maxStreak  = longestStreakHelper (currentStreak + 1) maxStreak (n-1)
    | sales n >= sales (n-1) && currentStreak >= maxStreak = longestStreakHelper (currentStreak +1) (currentStreak+1) (n-1)
    | sales n < sales (n-1)                                = longestStreakHelper 0 maxStreak (n-1)


-- (ii)


