-- Lecture 4a part 2
-- 4A (6) Symbolic Expressions

import Test.QuickCheck
import Data.Maybe(fromJust)

data Expr 
    = Num Integer
    | Add Expr Expr
    | Mul Expr Expr
    | Var String
    deriving (Eq)


instance Show Expr where
    show = showExpr -- this is needed so we can print nicely by simply typing "ex1 " in the console

ex1 = Mul (Add (Var "y") (Num 2)) (Var "x")
ex2 = Add (Var "x") (Mul (Num 2) (Var "y"))
ex3 = Num (-5) `Add` (Num 2 `Mul` Num 4)

vars :: Expr -> [String]
vars (Num n) = []
vars (Var s) = [s]
vars (Add e1 e2) = vars e1 ++ vars e2
vars (Mul e1 e2) = vars e1 ++ vars e2


type Table = [(String, Integer)]
eval :: Table -> Expr -> Integer
eval t e =  eval' e where
    eval' (Num n)     = n
    eval' (Mul e1 e2) = eval' e1 * eval' e2
    eval' (Add e1 e2) = eval' e1 + eval' e2
    eval' (Var x)     = fromJust $ lookup x t

    look k []          = error $ "No value for" ++ k
    look k ((k', v):t) | k == k'   = v
                       | otherwise = look k t


-- 4A (7)

-- data Maybe a = Nothing | Just a

-- 4A(8) The Generator

showExpr :: Expr -> String
showExpr (Var x) = x  -- x is already a string
showExpr (Num n) = show n
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2

showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e
                     


range = 4
level = fromInteger range
rExpr :: Int -> Gen Expr
rExpr s=  frequency  [(1,rNum),(s,rBin s),(1,rVar)] -- We add a size parameter since there is chance of it never stopping otherwise
    where
        rVar = elements $ map Var ["x","y","z"]
        rNum = elements $ map Num [-range..range] -- range
        rBin s = do 
            let s' = (s `div` 2)
            op <- elements [Mul,Add]
            e1 <- rExpr s'
            e2 <- rExpr s'
            return $ op e1 e2


instance Arbitrary Expr where
    arbitrary = sized rExpr

-- 4A (9) Derivation using smart constructors
derive :: String -> Expr -> Expr
derive x (Add e1 e2)      = add (derive x e1) (derive x e2)
derive x (Mul e1 e2)      = add (mul(derive x e1) e2) (mul e1 (derive x e2)) --product rule
derive x (Var y) | x == y = Num 1
derive _ _                = Num 0

-- smart adder :3
add (Num n) (Num m) = Num (n+m)
add (Num 0) e       = e
add e       (Num 0) = e
add e1      e2      = Add e1 e2

--smart mul

mul (Num n) (Num m) = Num (n*m)
mul (Num 0) e       = (Num 0)
mul e       (Num 0) = (Num 0)
mul (Num 1) e       = e
mul e       (Num 1) = e
mul e1      e2      = Mul e1 e2