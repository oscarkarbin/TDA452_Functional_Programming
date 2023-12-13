import System.Random(randomRs,mkStdGen)
import Data.List((\\))
import Test.QuickCheck

-- The Swedish Cake problem
-- Difficulty 🌶🌶

-- A cake is left in the common room for everyone to enjoy.
-- People never take more than one piece, but obey the
-- following protocol:

-- If there is more than 150g then they take a 100g slice.
-- If there is less than 30g then they take it all.
-- If there is between 30 and 150g then they take half of the remaining cake.

-- Write a recursive function to compute the number
-- of people who will get a taste of a cake that
-- weighs x grams.
-- Try to write the type of the function before you start. 

cake :: Float -> Int
cake n | n < 0   = error "cake: antimatter detected"
       | n < 30  = 1
       | n > 150 = 1 + cake (n - 100)
       | otherwise = 1 + cake (n/2)


-------------------------------------------------------
-- This questions is about the weekly sales from a shop,
-- where week numbers start at zero. 

-- sales i is the sales for week i.
-- Assume the week number (Int) is positive.
-- Sales is always non-negative.

sales :: Int -> Integer
sales i = randomRs (0,1000) (mkStdGen i) !! i
           -- sales in the range 0 - 1000
           -- The definition is not important!


-- Difficulty 🌶️Give recursive definitions of functions
-- which compute:

-- (1) total sales for the first n weeks?

totalSales n | n < 0     = 0
             | otherwise = sales n + totalSales (n-1)


-- Note that taking the base case to be negative weeks simplifies the code a little
-- as we don't repeat sales n, but now the code gives zero for negative week
-- numbers, which is consistent with the next definition:

totalSales' n = sum [sales m | m <- [0..n]]

-- (2) highest sale in the first n weeks?

highest n = sales 0
highest n = max (sales n) (highest (n-1))

highest' n = maximum [sales m | m <- [0..n]]

-- Note that this function gives an error for negative weeks.
-- One could argue that we could give 0 since sales are assumed never negative
-- (0 is then the identity element for the max operator)


-- (3) number of weeks with sales less than 100
--     in the first n weeks. 

countLowSales n | n < 0     = 0
                | otherwise = lowSale n + countLowSales (n - 1)
   where 
   lowSale m | sales m < 100 = 1
             | otherwise     = 0


-- (4) Define each of these using list comprehensions
--     instead of recursion

countLowSales' n = length [() | m <- [0..n], sales m < 100]


-- Using recursion, indirectly or in a helper function:
-- (4) Average sales up to and including week n?

averageSales0 n = tot/num
  where tot = fromIntegral (totalSales n)
        num = 1 + fromIntegral n

-- Not sure if this is what I had in mind when I created this exercise... 

-- (5) Give the week numbers of the weeks with the best
-- sales in the first n weeks.
-- Since there may be more than one week with the best sales,
-- your answer should be a list of weeks.

-- The tricky part here is to avoid too much cut-an-paste computation.
-- Another mistake is to compute highest on each step of the computation.

bestSales n = best n
  where  
    high = highest n
    best m | m < 0     = []
           | otherwise = [m | sales m == high] ++ best (m - 1)

-- Note a cute trick here: a list comprehension without a generator!
-- 
prop_comprehension e p = [ e | p ] == if p then [e] else []

-- Note also that without the local definition of high, we easily get an O(n^2) computation,
-- as we can recompute highest n at each step. 

-- Without recursion it is easier:
bestSales' n  = let high = highest n in [m | m <- [0..n], sales m == high]

-- Or with filter:
bestSales'' n = let high = highest n in filter (\ m -> sales m == high) [0..n]


-- (6) Write a quickcheck property for (5)
-- which checks that you included all the right weeks
-- in your answer, and none of the wrong ones!

-- By the time you get to this you should have seen higher-order functions etc.
-- This is painful to write just with recursion, but also the property starts too look
-- too similar to the code you are testing, which is not so useful!  I could not face
-- writing a recursive version, but it is a nice exercise without :)

prop_bestSales m  =
      let n          = abs m 
          high       = highest n
          bestWeeks  = bestSales n
          otherWeeks = [0..n]\\bestWeeks
        in  all (==high) (map sales bestWeeks) &&  -- none wrong
            all (<high)  (map sales otherWeeks)    -- all right








