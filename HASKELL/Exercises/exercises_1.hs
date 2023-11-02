import System.Random(randomRs,mkStdGen)
--Problem 1: The swedish cake problem
{-
If there is more than 150g then they take a 100g slice.
If there is less than 30g then they take it all.
If there is between 30 and 150g then they take half of the remaining cake.
-}

cakeSplit :: Integer -> Integer
cakeSplit 0 = 0 
cakeSplit n | n > 150   = 1 + cakeSplit(n-100)
            | n < 30    = 1 + cakeSplit(0)
            | otherwise = 1 + cakeSplit(n `div` 2)

-------------------------------------------------------------------


--Problem 2: Weekly sales
sales :: Int -> Integer
sales i = randomRs (0,1000) (mkStdGen i) !! i


--A-----------------------------
totalSales :: Int -> Integer
totalSales 0 = 0
totalSales n = sales (n) + totalSales (n-1)
-----------------------------------

--B---------------------------------
highestSales :: Int -> Integer
highestSales 0 = 0
highestSales n = biggerOrLower n 0 0 -- Weeks, currentWeek, Highets
    where 
        biggerOrLower 0 _ hi = hi
        biggerOrLower weeks currentWeek hi = biggerOrLower (weeks - 1) (currentWeek + 1) (biggest (hi) (sales (currentWeek)))
            where
                biggest s1 s2 | s1 >= s2  = s1
                              | otherwise = s2
-----------------------------------------------------

--C---------------------------------------------
badPerformace :: Int -> Integer
badPerformace 0 = 0
badPerformace n | sales(n) < 100 = 1 + badPerformace(n-1)
                | otherwise      = badPerformace(n-1)
------------------------------------------------

--D-------------------------------------------------

totalSales2 :: Int -> Integer 
totalSales2 n = sum [sales i | i <- [0..n]]

highestSales2 :: Int -> Integer
highestSales2 n = maximum [sales i | i <- [0..n]]

badPerformace2 :: Int -> Int
badPerformace2 n = length [ i | i <- [0..n], sales i < 100]

-------------------------------------------------------

--E- Average sales up to and including week n?-------

avgSales :: Int -> Integer
avgSales 0 = 0
avgSales n = (totalSales2 n) `div` fromIntegral n

------------------------------------

--F Give the week numbers of the weeks with the best sales in the first n weeks. Since there may be more than one week with the best sales, your answer should be a list of weeks.----
