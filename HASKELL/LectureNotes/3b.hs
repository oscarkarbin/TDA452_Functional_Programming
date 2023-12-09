import Test.QuickCheck



-- 3B (1) Type Classes and Overloading

{-
ex1:

Num a => a -> a -> a


Type classes declare a set of methods, ie set of related overloaded functions and operators.

-}

-- defining my own instances

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red    == Red       = True
    Yellow == Yellow    = True
    Green  == Green     = True
    _      == _         = False

-- ex 2 defining my own show instance

-- data Suit = Spades | Hearts | Diamonds | Clubs
--     deriving (Eq, Ord, Enum, Bounded)

-- instance Show Suit where
--     show Spades   = "spades symbol"
--     show Hearts   = "Hearts symbol"
--     show Diamonds = "Diamond sym"
--     show Clubs    = "Club symbol"

-- The reason why we are talking about overloading is because it is need for Test data generators. 
{-
QuickCheck can perform testing on values in class Arbitrary

any type T in Arbitrary there is a random value generator
arbitrary :: Gen T

Gen ===> "QuickCheck instructions for creating a T"

-}

--3B (2) Do notation is not just for IO
-- IO
-- do notation to build "larger" IO functions

--getLine :: IO String

getName :: IO String
getName = do
    putStrLn "Please type name:"
    a <- getLine
    return(a)

-- the type of the entire do-block will be "IO-String" beacuse the last line of the do block is a getLine which is a "IO-String"

-- doTwice :: IO a -> IO (a, a)
doTwice io = do   -- we give it an IO and it will do it twice. 
    a <- io
    b <- io
    return(a,b)

{-

:t doTwice (getName)
   ==> 
doTwice (getName) :: IO (String, String)

:t doTwice
 ==>
doTwice :: Monad m => m b -> m (b, b)        For now we can think of this any type of "Instructions"

-}

-- 3B (3) QuickCheck Generators - arbitrary and sample

{-
IO vs Gen

IO:
    - Instructions to build a value of type T by interacting with the operating system
    - Run by the ghc runtime system

Gen:
    - Instructions to create a random value of type T
    - Run by the QuickCheck library functions to perform random tests 


sample ==> Will create 10 random item of type "a" using quickchekc (arbitrary :: Gen a)
sample' ==> will do the same as sample, however it will place the items in a list
-}


--3B (4) Building generators with do notation

--writing our own generators using return, do

--natural numbers

nats :: Gen Int
nats = do
    n <- arbitrary
    return(abs n)

nats1 :: Gen Integer
nats1 = abs <$> arbitrary


-- even numbers

even :: Gen Integer
even = do
    n <- arbitrary
    return (2 * n)

even1 :: Gen Integer
even1 = (2*) <$> arbitrary

evenNats :: Gen Integer
evenNats = (2*) <$> abs <$> arbitrary

-- 3B (5) QuickCheck functions - listOf vectorOf choose oneof elements

-- Buildning Generators: Library functions
{-
    - listOf, listOf1, vectorOf                   (listOf1 creates only non empty lists)
    - choose (?,?)
        *Randomly chooses a an item in a range (?,?) ex: choose(1,9)
    - return
        *Will turn a value into a Gen (of that value). ex int -> Gen int
    - oneof
        *Give it a list of different generators and it will pick one of them to generate a random value.   
        ex: sample $ oneof [return 42, choose (-1,1), even1]
    - elements
        * elements :: [a] -> Gen a
        we give a it a list and it picks one item in the list randomly and turns it into a Gen (of that item)
    - frequency
-}

--3B (6) Example - Gen Rank and Gen Suit

--ex code from lab2
data Suit = Hearts | Spades | Diamonds | Clubs
            deriving (Eq, Show)

--making my own instance
instance Arbitrary Suit where
    arbitrary = elements [Spades, Hearts, Diamonds, Clubs]

-- data type that represents cards
data Rank = Numeric Integer | Jack | Queen | King | Ace
            deriving (Eq, Show, Ord)

-- we create two generators here. One for the numeric case 
-- another one for the royal case
rNumeric, rRoyal :: Gen Rank
rNumeric = do
    n <- choose (2,10)
    return (Numeric n)  -- Numeric <$> choose (2,10)

rRoyal = elements [Jack, Queen, King, Ace]

instance Arbitrary Rank where
    arbitrary = frequency [(9,rNumeric),(4,rRoyal)]
 -- arbitrary = oneof [rNumeric,rRoyal]

prop_rank (Numeric n) = n <= 10 && n >  1
prop_rank _           = True

prop_rank' r = classify (r < Jack) "Numeric"  -- classify will identify generated ranks that are less than a jack (All numeric)
             $ prop_rank r                      -- This is then displayd when runnin quickCheck
