


-- 5B (1) Monads - The monad typeclass

-- IO: Instructions for interacting with the operating system
{-
do
    s <- getLine
    c <- readFile s
    return $ s ++ c
-}

-- Gen: Instructions for creating random variables
{-
do
    n <- elements [1..9]
    m <- vectorOf n arbitrary
    return $ n:m
-}

-- Parser: Instructions for parsing
{-
do
    c <- sat (`elem` ";,:")
    ds <- chain digit (char c)
    return $ map digitToInt ds
-}

{-
"Monadic value" is just an expression whose type is an instance of class monad
-}

-- Monads and do notations
-- * >>= and return

{-
do == is just a syntax shorthand for
    * do act1; act2 ==> act1 >> act2 ==> act1 >>= \_ -> act2
    * do            ==> act1 >>= \v -> act2
        v <- act1
        act2 v
-}

take10 = do 
    filename <- getLine
    contents <- readFile filename
    putStr(take 10 contents)


-- Maybe is also a monad!!!
-- A very simple