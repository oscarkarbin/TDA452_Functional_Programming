import System.Random(randomRs,mkStdGen)
import Test.QuickCheck
{-
1. The Swedish Cake problem. Difficulty 🌶️🌶️

A cake is left in the common room for everyone to enjoy. People never take more than one piece, 
but obey the following protocol:

If there is more than 150g then they take a 100g slice.
If there is less than 30g then they take it all.
If there is between 30 and 150g then they take half of the remaining cake.
Write a recursive function to compute the number of people who will get a taste of a cake that weighs x grams.
Try to write the type of the function before you start. 
-}

swedishCake :: Integer -> Integer -- grams of cake -> amount of people
swedishCake 0 = 0
swedishCake g | g > 150 = swedishCake (g-100) + 1
              | g < 30  = swedishCake 0 + 1
              | otherwise = swedishCake(g `div` 2) + 1


{-
This questions is about the weekly sales from a shop, where week numbers start at zero. 

Sales i is the sales for week i.
Assume the week number (Int) is positive.
Sales is always non-negative.

-}

sales :: Int -> Integer
sales i = randomRs (0,1000) (mkStdGen i) !! i
           -- sales in the range 0 - 1000
           -- The definition is not important!

{-
Difficulty 🌶️
Give recursive definitions of functions which compute:

(a) total sales for the first n weeks?
(b) highest sale in the first n weeks?
(c) number of weeks with sales less than 100 in the first n weeks. 
(d) Define each of these using list comprehensions instead of recursion

Using recursion, indirectly or in a helper function:
-}

-- a)
totSales :: Int -> Integer
totSales 0  = sales 0
totSales wk = sales wk + totSales (wk-1)

-- b)

maxSales :: Int -> Integer
maxSales 0 = sales 0
maxSales wk = sales wk `comp` maxSales (wk - 1)
    where
        (comp) a b | a > b     = a
                   | a < b     = b 
                   | otherwise = a

-- c)
badWeeks :: Int -> Integer
badWeeks 0 = 0
badWeeks wk | sales wk >= 100 = badWeeks(wk-1)
            | sales wk < 100  = badWeeks(wk-1) + 1

-- d)
  
totSales1 :: Int -> Integer
totSales1 wk = foldr (+) 0 [sales x | x <- [0..wk]] -- instead of foldr (+) 0 we can simply use sum.


maxSales1 :: Int -> Integer
maxSales1 wk = maximum [sales x | x <- [0..wk]]

badWeeks1 :: Int -> Int
badWeeks1 wk = length [x | x <- [0..wk], sales x < 100]

{-
Difficulty 🌶️🌶️
(e) Average sales up to and including week n?
-}

-- e)

avgSales' :: Int -> Integer
avgSales' 0 = sales 0
avgSales' wk = totSales wk `div` toInteger wk

{-Difficulty 🌶️🌶️🌶️
(f) Give the week numbers of the weeks with the best sales in the first n weeks.
    Since there may be more than one week with the best sales, your answer should be a list of weeks.

(g) Write a quickcheck property for (f) which checks that you included all the right weeks in your answer, 
and none of the wrong ones!-}

-- f) 

bestSales :: Int -> [Int] -- we want to include every week with sales larger than the avrage roughly 500
bestSales wk = [x | x <- [0..wk], sales x > 500]

-- g)

prop_moreThanAvg a = all (>500) [sales x | x <- bestSales a]