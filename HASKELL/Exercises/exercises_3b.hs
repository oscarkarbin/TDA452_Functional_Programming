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
    (x',y') <- arbitrary
    let x =abs x'
    let y = 2 * x + abs y'
    return (x,y)

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

{-
contains1 :: Arbitrary a => a -> Gen[a]
contains1 item = do
    front <- listOf arbitrary
    back <- listOf arbitrary
    return (front ++ [item] ++ back)

This is almost correct. However to make it entirely general we have to allow other custom Gen:
-}

genContains :: Gen a -> a -> Gen[a]
genContains gen item = do
    front <- listOf gen
    back <- listOf gen
    return(front ++ [item] ++ back)


contains1 num = arbitrary `genContains` num

-------------------------------------------------------------------------

{-6. Give a definition for the QuickCheck function elements. Hint: use arbitrary and choose.-}

elements1 :: [a] -> Gen a
elements1 list = do
    index <- choose (0 ,(length list) -1 )
    return $ list !! index

-- 7.

data Card = Card Rank Suit
      deriving (Eq, Show)

data Rank = Numeric Integer | Jack | Queen | King | Ace
            deriving (Eq, Show)

-- | All the different suits.
data Suit = Hearts | Spades | Diamonds | Clubs
            deriving (Eq, Show)

-- | A hand of cards. This data type can also be used to represent a
-- deck of cards.
data Hand = Empty | Add Card Hand
            deriving (Eq, Show)

shrink' Empty = []
shrink' (Add c h) = Empty : h : [Add c h' | h'<- shrink' h]

hand1 = Add (Card (Numeric 7) Diamonds) (Add (Card Queen Clubs) (Add (Card King Hearts) Empty))

    
    
    



