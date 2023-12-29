import Test.QuickCheck
import Data.Char (generalCategory)


{- 1.
Write a QuickCheck generator for pairs of positive integers, 
where the second number is at least twice as big as the first. 
Start by writing the type signature for your function.
-}

-- Answer 1. -------------------------------------------------

posPairs :: Gen (Integer, Integer)
posPairs = do
    n <- arbitrary
    mul <- choose(2,10)
    return (n, n*mul)

--------------------------------------------------------------

{- 2.
Write a recursive definition of the quickCheck function vectorOf.
-}

-- Answer 2. -------------------------------------------------

vectorOf1 :: Integer -> Gen a -> Gen [a]
vectorOf1 n gen = do 
    vectorHelper n gen []

vectorHelper :: Integer -> Gen a -> [a] -> Gen [a] 
vectorHelper 0 gen list = return(list)
vectorHelper n gen list = do
    item <- gen
    vectorHelper (n-1) gen (item:list)

--------------------------------------------------------------

{- 3.
Write a generator: prefixPair :: Gen (String, String) which 
generates pairs of strings where the first string is always a prefix of the second.-}

-- Answer 3. -------------------------------------------------

prefixPair :: Gen (String, String)
prefixPair = do
    (prefix, suffix) <- arbitrary
    return(prefix, prefix ++ suffix)


---------------------------------------------------------------

{-4.
Write a function
contains :: Int -> Gen [Int]

where for example, contains 7 is a generator for lists of Int which always contain at least one 7.-}

-- Answer 4. --------------------------------------------------

contains :: Int -> Gen[Int]
contains num = do
    (front, back) <- arbitrary
    return (front ++ [num] ++ back)

---------------------------------------------------------------

{- 5. Consider two ways to generalise contains, (
    i) by generalising the type 
        (the Haskell compiler can do this for you, but try to find the most general type for your definition), and 
    (ii) by making the function more general so that it is not overloaded*. -}

-- Answer 5. --------------------------------------------------





    

    
    
    



