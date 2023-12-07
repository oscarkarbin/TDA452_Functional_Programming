import Graphics.UI.Threepenny (a)

--6B Data Structures (1) Intro
-- we have looked at data types == Hand etc
{-
Data structure is the way we store data
-}


{-
Abstract data type

The operations we want to preform on the data. 

-}

-- 6B Data Structures (2) A Slow Queue

{-
What is a Queue?

Join at the back

===> 1 2 3 4 5 ==> leave at the front

ex:
    files to print
    processes to run
    tasks to perform
-}

--------------------------------------------------------------
-- Interface 

empty   :: Q a
add     :: a -> Q a -> Q a
remove  :: Q a -> Q a
front   :: Q a -> a
isEmpty :: Q a -> Bool

--------------------------------------------------------------
-- Implementtation

data Q a = Q [a]
    deriving (Eq, Show)

empty              = Q []
add a (Q as)       = Q (as ++ [a])
remove (Q (a:as))  = Q as
front (Q (a:as))   = a
isEmpty (Q as)     = null as

-- 6B Data Structures (3) A Fast Queue - Basic Idea

{-
Split the queue to a front and back. The front queue will be fast to remove and the
back queue will be fast to remove. We do this reversing the back queue to and only reverse it back to normal 
when the front queue is empty.
-}

-- 6B Data Structures (4) A Fast Queue Implementation

--------------------------------------------------------------
-- Interface (FAST)

empty1   :: Q1 a
add1     :: a -> Q1 a -> Q1 a
remove1  :: Q1 a -> Q1 a
front1   :: Q1 a -> a
isEmpty1 :: Q1 a -> Bool


-- Implementation

data Q1 a = Q1 [a] [a]
    deriving (Eq, Show)

empty1                    = Q1 [] []
add1 a (Q1 front back)    = fixQ front (a:back)
remove1 (Q1 (x:xs) back)  = fixQ xs back   
front1 (Q1 (x:_) _)       = x 
isEmpty1 (Q1 front back)  = null front && null back

fixQ [] back   = Q1 (reverse back) []
fixQ fron back = Q1 fron back

-- 6B Data Structures (6) Lookup Tables - Intro
-- Tables another datastructure

-- table lookup using lists:
-- type Table k v = [(k,v)]


--6B Data Structures (7) Lookup Tables Implementation
data Table k v
    = Join (Table k v) k v (Table k v)
    | Empty
    deriving(Eq, Show) 

lookupT :: Ord k => k -> Table k v -> Maybe v
lookupT k Empty = Nothing
lookupT k (Join tl k' v rt) 
    | k == k' = Just v
    | k < k'  = lookupT k tl
    | k > k'  = lookupT k rt

-- To look up a key in a table:
{-
if the table is empty, then the key is not found

compare teh key with the key of the middle elemnt

if they are equal, return the associated value

if key us less. Look in the first half

if the key is greater look in the second half of the table.
-}

-- Inserting a new key
-- Be carful when inserting so order is maintained

insertT :: Ord k => k -> v -> Table k v -> Table k v
insertT k v Empty = Join Empty k v Empty
insertT k v (Join lt k' v' rt)  
    | k == k' = (Join lt k v rt)
    | k < k'  = Join (insertT k v lt) k' v' rt
    | k > k'  = Join lt k' v' (insertT k v rt)
