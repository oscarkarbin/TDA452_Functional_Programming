-- Lecture 4A
import Test.QuickCheck
--4A(1) Recursive Data Types - Arithmetic Expressions

data Expr 
    = Num Integer
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Eq)


--example (1+2)*4
ex1 = Mul (Add (Num 1) (Num 2)) (Num 4)

--example 1+2*4
ex2 = Add (Num 1) (Mul (Num 2) (Num 4))

--exal :: Expr -> Int
eval :: Expr -> Integer
eval (Num n) = n
eval (Mul e1 e2) = eval e1 * eval e2
eval (Add e1 e2) = eval e1 + eval e2

--4A (2) Showing Arithmetic Expressions
-- show the expression
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2

showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

--4A (3) Making an Manual Instance of Show

instance Show Expr where
    show = showExpr
-- 4A (4) QuickCheck Generators for Recursive Data - sized

range = 4
level = fromInteger range
rExpr :: Int -> Gen Expr
rExpr s=  frequency  [(1,rNum),(s,rBin s)] -- We add a size parameter since there is chance of it never stopping otherwise
    where
        rNum = elements $ map Num [-range..range] -- range
        rBin s = do 
            let s' = (s `div` 2)
            op <- elements [Mul,Add]
            e1 <- rExpr s'
            e2 <- rExpr s'
            return $ op e1 e2

instance Arbitrary Expr where
    arbitrary = sized rExpr
-----------------------------------------------------------------------------
main :: IO()
main = do
    --e <- rExpr level
    es <- sample' $ rExpr level 
    let e = es !! level
    putStrLn $ "What is the value of" ++ show e
    ans <- getLine
    if (ans == (show $ eval e))
        then putStrLn "Correct!"
        else putStrLn $ "Fail! Correct answer was:" ++ (show $ eval e)
    main
-----------------------------------------------------------------------------
