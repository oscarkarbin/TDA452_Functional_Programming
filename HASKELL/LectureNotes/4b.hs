import Data.Char


-- 4B (1) What is Lazy Evaluation

-- Cheap or Expensive
-- this is a bad function
expensive :: Integer -> Integer
expensive n
    | n <= 1 = 1
    | otherwise = expensive (n-1) + expensive (n-2)

{-
Rewriting:
    Haskell will reduce expressions
    1+1 = 2

    Unfolding 
    head (e1:e2) -> e1

    Any expression that can be rewriten is called a redex.

    Haskell will rewrite the outermost reduction. head((1+1), etc, etc). 
    HASKELL: call-by-name

    Lazy Evaluation  == call-by-name + sharing (call by need)
-}

--4B (2) Lazy Style

-- Laziness allows us to work with infinite lists => take 5 [1..]

-- Backtracking a type of search algo. With lazy evaluation we can search for solutions without searching. 

primes :: [Integer]
primes = sieve [2..]

sieve (x:xs) =
    x: sieve [y | y <- xs, y `mod` x /= 0]

-- These are examples of using infinte lists in modular programming.
nprimes n = take n primes -- the first n primes

primesBelow n = takeWhile (<n) primes -- all primes below n

primesRange m n = dropWhile (<=m) $ primesBelow n -- all primes in range

twinPrimes = filter twins (zip primes (tail primes)) -- twin primes
    where twins (x,y) = y == x + 2

-- iterate, repeat, cycle
-- iterate :: (a->a) -> a -> [a]
-- repeat :: a -> [a]
-- cycle :: [a] -> [a]

-- 4B (3) Lazy IO

-- interact :: (String -> String) -> IO()


-- 4B (4) Lazy Data Structures

data Labarynth = Crossroad String Labarynth Labarynth

what (Crossroad place _ _) = place
right (Crossroad _ _ rightPlace) = rightPlace
left (Crossroad _ leftPlace _) = leftPlace

-- 4B (5) Tail Recursion

{- Controlling Laziness

* We can controll laziness 
* USed for performace tuning
-}

-- This will have a stack overflow eventually.
-- 1 (2+(3+(4........))) we will run out of stack
sum0 []     = 0
sum0 (n:ns) = n + sum0 ns

-- Tail recursive would solve this in other languages.
sum1 = s 0 
    where s acc []       = acc
          s acc (n:ns) = s (n+acc) ns -- Beacause haskell is lazy (n+acc) isn't calculated until we have parsed thorugh the entire list
                                        -- This means that we have the same problem as above. We have only moved it from the stack to somewhere else. (heap??)


-- 4B (6) Controlling laziness with seq ()
-- Lazy evaluation messes still gets in the way.

-- seq :: a -> b -> b
-- seq forces computation

-- prelude also defines
-- ($!) :: (a -> b) -> a -> b
-- f $! x = x `seq` f x

sum2 = s 0 
    where s acc []       = acc
          s acc (n:ns) = acc `seq` s (acc+n) ns 

-- strict tail recursion
-- foldl' ==> Data.List(foldl')

 -- 4B (7) Problems with lazy IO 



shoutify :: FilePath -> IO String
shoutify f = do 
    c <- readFile f
    let shout = map toUpper c 
    last shout `seq` writeFile f shout -- seq is quite lazy still so we need to have "last" 
    return shout                                   