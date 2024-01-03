{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

import Data.List (nub)
import Test.QuickCheck
import Test.QuickCheck (choose)





data B = W B B B | X Int | Y B
    deriving Show;

{- 1.
Give a definition of a value of type B which contains at least one of each constructor.
-}

exampleValue = Y (W (Y (X 2)) (X 3) (X 3))



{- 2.
Define a (non-recursive) function isX :: B -> Bool which tests whether the given B is built with an X constructor (on the outside).-}

isX :: B -> Bool
isX (X _) = True
isX _     = False

{- 3.
Define the typical “template” for a recursive function over type B
-}

{-
template :: B -> ???
template (X _)        = ...
template (W b1 b2 b3) = ... template b1 ... template b2 ... template b3
template (Y b)        = ... template b
-}

{- 4.
Define a function containsZero :: B -> Bool which gives True only if the argument contains a zero somewhere inside (and gives False otherwise).
-}

containsZero :: B -> Bool
containsZero (X 0) = True
containsZero (X _) = False
containsZero (Y b) = containsZero b
containsZero (W b1 b2 b3) = containsZero b1 || containsZero b2 || containsZero b3

{- 5.
Define sumB :: B -> Int which sums all the numbers in the B.
-}

sumB :: B -> Int
sumB (X n) = n
sumB (Y b) = sumB b
sumB (W b1 b2 b3) = sumB b1 + sumB b2 + sumB b3

{- 6. 
Define allInts :: B -> [Int] which gives a list of all the numbers in a given B but without duplicates 
(hint: you might find a useful function of type Eq a => [a] -> [a] or perhaps one of type Eq a => a -> [a] -> Bool that could help you solve this. 
hoogle itLinks to an external site.!).-}

allInts :: B -> [Int]
allInts (X n) = [n]
allInts (Y b) = nub (allInts b)
allInts (W b1 b2 b3) = nub (allInts b1 ++ allInts b2 ++ allInts b3)

{- 7.
Define a function doubleB :: B -> B which returns the B obtained by doubling every number in the given B.-}

doubleB :: B -> B
doubleB (X n) = X (n*2)
doubleB (Y b) = Y (doubleB b)
doubleB (W b1 b2 b3) = W (doubleB b1) (doubleB b2) (doubleB b3)

{- 8.
-}

data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

eval :: Expr -> Int
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

showExpr :: Expr -> String
showExpr (Lit n)     = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ "*" ++ showExpr e2 ++")"
showExpr (Div e1 e2) = "(" ++ showExpr e1 ++ "/" ++ showExpr e2 ++")"

{-
B(*). Define the function ==> size :: Expr -> Int ,
which counts the number of operators in an expression.
-}

size :: Expr -> Int
size (Lit _) = 0
size (Add e1 e2) = 1 + size e1 + size e2
size (Sub e1 e2) = 1 + size e1 + size e2
size (Mul e1 e2) = 1 + size e1 + size e2
size (Div e1 e2) = 1 + size e1 + size e2

{-
C(*). Add the operations of multiplication and integer division to the type Expr, and redefine the functions eval, showExpr, and size 
to include the new cases. What does your eval do when you divide by zero? Write one version of eval with the result type Maybe Int.
-}

evalMaybe :: Expr -> Maybe Int
evalMaybe (Lit n)     = Just n
evalMaybe (Add e1 e2) = (+) <$> evalMaybe e1 <*> evalMaybe e2
evalMaybe (Sub e1 e2) = (-) <$> evalMaybe e1 <*> evalMaybe e2
evalMaybe (Mul e1 e2) = (*) <$> evalMaybe e1 <*> evalMaybe e2
evalMaybe (Div e1 e2) = do v1 <- evalMaybe e1
                           v2 <- evalMaybe e2
                           if v2==0 then Nothing else Just (v1 `div` v2)


-- Integer Trees

data NTree = NilT
           | Node Int NTree NTree
           deriving (Eq,Show);

sumTree :: NTree -> Int
sumTree NilT           = 0
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

depth :: NTree -> Int
depth NilT           = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

{-
B(*). Define functions to return the left- and right-hand sub-trees of an NTree.
-}

exTree = (Node 3 (Node 4 NilT NilT) NilT)

leftTree :: NTree -> NTree
leftTree NilT = NilT
leftTree (Node n left right) = left

rightTree :: NTree -> NTree
rightTree NilT = NilT
rightTree (Node n left right) = right

{-
C(*). Define a function to decide whether a number is an element of an NTree.
-}

numberIn :: Int -> NTree -> Bool
numberIn n (NilT) = False
numberIn n (Node num left right) = n == num || numberIn n left || numberIn n right

{-
E(*). A tree is reflected by swapping left and right sub-trees, recursively. 
Define a function to reflect an NTree. What is the result of reflecting twice? Write a QuickCheck property for that!
-}

reflectTree :: NTree -> NTree
reflectTree NilT = NilT
reflectTree (Node n left right) = Node n (reflectTree right) (reflectTree left)

rNilt :: Gen NTree
rNilt = do
    return(NilT)

rTree :: Gen NTree
rTree = do
    n <- choose(1,10)
    l <- frequency[(6,rNilt),(4,rTree)]
    r <- oneof[rNilt,rTree]
    return (Node n l r)

instance Arbitrary NTree where
    arbitrary = rTree

prop_reflectTree t1 = t1 == reflectTree (reflectTree t1) 

{-
1 (*). File Systems

A. Design a data type to represent the contents of a directory. 
Ignore the contents of files: you are just trying to represent file names and the way they are organised into directories here.
Solution

B. Define a function to search for a given file name in a directory. 
You should return a path leading to a file with the given name. 
Thus if your directory contains a, b, and c, and b is a directory containing x and y, then searching for x should produce b/x.
Solution
-}

-- A

data Dir = File String | Dir [(String,Dir)]




-- B ??????????


