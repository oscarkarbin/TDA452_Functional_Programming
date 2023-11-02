--LECTURE NOTES 1B
{-IMPORTANT NOTES

    -   set warning grejen

    -   The name of types should start with uppercase

    -   Three type classes 
        * Show = To print in the system io
        * Eq   = To identify if things/data types are equal
        * Ord  = To identify the ordering

    - When creating new data types we can derive (deriving) the abobe type classes. Haskell will choose obvious function within these type classes for your own data type.


    - Hoogle https://hoogle.haskell.org/


    - In order to use quickcheck with your own data types you need to tell it how it can generate random types using your data type.




-}
-- TEST
test n p = p + n 
--
-- Modelling and Datatypes
{-
A big part of designing software is modelling the data in appropriate way

A lot of data can be represented as numbers or strings, but there is often better alternatives.

Todays lecture: we look at how to model data by defining NEW types in Haskell.
-}

--Example: Modelling playing cards
data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq) -- With this type class 'Show' we can print the  
-- The name of types should start with uppercase

-- Colour
--Colour (eq)

colour :: Suit -> Colour
colour Spades = Black
colour Clubs  = Black
colour _      = Red -- The underscore '_' is called the don't care pattern


data Colour = Red | Black
    deriving (Show, Eq)

colorCard c  = colour (suit c)

--1B(2) MORE COMPLEX TYPES
data Rank  = Numeric Int | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord) 

--Numeric Int means that each Numeric (any number) belongs to the data type Rank.

--1B(3) 

--Datatype invariants

prop_Rank (Numeric n) = n > 1 && n <= 10
prop_Rank _           = True

--1B(4) RANK BEATS RANK
{-  This is an example on how to implement rankBeats. However it is quite a large definintion and requires testing
rankBeats :: Rank -> Rank -> Bool
rankBeats _ Ace = False     --nothing beats an ace
rankBeats Ace _ = True      -- an ace beats everything
rankBeats _ King = False
rankBeats King _ = True
rankBeats _ Queen = False
rankBeats Queen _ = True
rankBeats _ Jack = False
rankBeats Jack _ = True
rankBeats (Numeric m) (Numeric n) = m > n
-}


-- We can create this rankBeats function with Eq and Ord!!!
rankBeats  :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 > r2

prop_rankBeats r1 r2 = rankBeats r1 r2 || rankBeats r2 r1 || r1 == r2


--1B(5)

-- Card: a data type containing a Rank and a Suit and it projection function

data Card = Card Rank Suit
    deriving (Show,Eq)

suit :: Card -> Suit
suit (Card r s) = s


rank :: Card -> Rank
rank (Card r s) = r

-- When does one card beat another card fucntion below:
cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = s1 == s2 && r1 `rankBeats` r2

--1B (6) RECURSIVE DATA TYPES

david = Card King Hearts
death = Card Ace Spades
--------------------------------
--Hand: datatype for a hand of cards

--type Hand = [Card]    --This is okay but we will define our own datatypes

data Hand = Empty | Add Card Hand -- This is recursicve !!!
    deriving Show

example1 = Add david Empty
example2 = Add death example1   -- Example of hands

-- Simple ex: size of hand

size :: Hand -> Int
size Empty     = 0
size (Add c h) = 1 + size h -- recursion works great with recursive data types

len :: [a] -> Int
len[]      = 0
len (x:xs) = 1 + len xs

--handsBeats example: does one hand beat a given card (Does my hand have a card that beats  the card on the table????)
handBeats :: Hand -> Card -> Bool
handBeats Empty c = False
handBeats (Add c' h) c = c' `cardBeats` c || h `handBeats` c

-- 1B (7) Three ways to define a recursive functions over Hand

aHand = Add death (Add david Empty)


--1. Simple double traversal
splitHand :: Hand -> (Hand, Hand)
    --split a deck into the reds and blacks.
splitHand h = (select Red h, select Black h)


select :: Colour -> Hand -> Hand
select col Empty     =  Empty
select col (Add c h) | col == colorCard c = Add c (select col h)
                     | otherwise          = select col h

--2. Direct recursive definition
-- the trick here is to give a name to the splithand function (row 160). So when doing a direct recursion to a pair of things always give a name to the answer you expect to get 
-- from the recursive call.
splitHand2 Empty = (Empty, Empty)
splitHand2 (Add c h) | colorCard c == Red = (Add c reds, blacks)
                    | otherwise          = (reds, Add c blacks)
    where (reds, blacks) = splitHand2 h 

--3. Single recursion using a helper function (accumlating parameters)

splitHand3 h = split h Empty Empty
    where split Empty     reds blacks = (reds , blacks)
          split (Add c h) reds blacks
                | colorCard c == Red = split h (Add c reds) blacks
                | otherwise          = split h reds (Add c blacks)