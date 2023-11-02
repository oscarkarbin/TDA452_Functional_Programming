import Test.QuickCheck
{- Lab 1
   Date: 2023-10-31
   Authors: Oscar Karbin and Jihad Almahal
   Lab group: Group 6
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower x y 
   | y < 0 = error "negative value"
   | otherwise = y + 1


-- B -------------------------
-- power1

power1 :: Integer -> Integer -> Integer
power1 n k = product ([n | x<-[1..k]])

-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k 
   | even k = ((n*n)^(k `div` 2))
   | otherwise = (n*(n^(k-1)))

-- D -------------------------
{- 

7^0: This test case checks the functionality of the power function when the exponent is 0. Any number raised to the power of 0 is 1. 
This is a fundamental property and it's important that our functions adheres to this.

0^2: This case tests the function when the base is 0 and the exponent is positive. The expected result is 0, as zero raised to any positive power remains zero. 

0^0: Mathematically, 0^0 is considered indeterminate in many contexts. However in our program 0^0 is defined to be 1.

5^3: This is a straightforward case with a positive base and a positive exponent. It tests the function's ability to handle basic power calculations.

5^1: This case checks if the function correctly understands that any number raised to the power of 1 is the number itself. It's basic but important.

1^5: This tests the function with a base of 1. 1 raised to any power should result in 1.

5^(-2): This case introduces a negative exponent. The expected result is the reciprocal of the base raised to the positive version of the exponent (in this case, 1/5^2). 
However our functions are not intended to work with negative exponents. This test will act as an indicator that our functions gives correct errors.

(-5)^3: This case introduces a negative base with an odd exponent. The expected value is to be a negative number.  

 -}

-- 
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k == power1 n k) && (power1 n k == power2 n k)

--
powerTest :: [Bool]
powerTest = [prop_powers (fst x) (snd x) | x <- testCases]
  where
    testCases = [(7,0),(0,2),(0,0),(5,3),(5,1),(1,5),((-5),3),(5,(-2))]  -- Add more if needed

--We expect the last testCase to throw exception because it has a negative exponent. All other test cases pass!

prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = (power n k' == power1 n k') && (power1 n k' == power2 n k')
   where k' = abs k