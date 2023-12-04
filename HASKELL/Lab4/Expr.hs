import Parsing

-- A --------------------------------------------------
data Expr
    = Num Double
    | AddMul Opp Expr Expr
    | X
    | Func SinCos Expr
    deriving Eq

data SinCos = Sin | Cos
    deriving Eq

data Opp = Add | Mul
    deriving Eq

instance Show Expr where
    show = showExpr -- this is needed so we can print nicely by simply typing "ex1 " in the console

x :: Expr
x = X

num :: Double -> Expr
num n = Num n

add,mul :: Expr -> Expr -> Expr
add e1 e2 = AddMul Add e1 e2
mul e1 e2 = AddMul Mul e1 e2

sin,cos :: Expr -> Expr
sin e = Func Sin e

cos e = Func Cos e

-- (1+2) * x
ex1 = AddMul Mul (AddMul Add (Num 1.0) (Num 2.0)) X


size :: Expr -> Int
size (Num n)          = 0
size X                = 0
size (AddMul _ e1 e2) = 1 + (size e1 + size e2)
size (Func _ e1)      = 1 + size e1
-- END A --------------------------------------------------------------------------


-- B ------------------------------------------------------------------------------

{-
When showing and reading expressions, we have to decide where we allow and require parentheses. 
Parentheses are required only in the following cases:

When the arguments of a *-expression use +. For example: (3.1+4.2)*7
When the argument of sin or cos uses * or +. For example: sin (3.2*x)

data Expr
    = Num Double
    | AddMul String Expr Expr
    | X
    | Func String Expr
    deriving (Eq, Show)
-}

showExpr :: Expr -> String
showExpr (Num n)            = show n
showExpr (AddMul Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (AddMul Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showExpr (Func Sin e1)    = "sin" ++ showFunc e1
showExpr (Func Cos e1)    = "cos" ++ showFunc e1
showExpr X                  = "x"

showFactor :: Expr -> String
showFactor (AddMul Add e1 e2) = "(" ++ showExpr (AddMul Add e1 e2) ++ ")"
showFactor e                  = showExpr e

showFunc :: Expr -> String
showFunc (AddMul op e1 e2) = "(" ++ showExpr (AddMul op e1 e2) ++ ")"
showFunc e                = showExpr e

-- END B----------------------------------------------------------------

-- C ---------------------------------------------------------------------

eval :: Expr -> Double -> Double
eval (Num n) x            = n
eval X x                  = x
eval (AddMul Add e1 e2) x = eval e1 x + eval e2 x
eval (AddMul Mul e1 e2) x = eval e1 x * eval e2 x
eval (Func Sin e1) x      = Prelude.sin (eval e1 x) 
eval (Func Cos e1) x      = Prelude.cos (eval e1 x)


-------------------------------------------------------------------------------

-- D -------------------------------------------------------------------


exprParser :: Parser Expr
exprParser = undefined


readExpr :: String -> Maybe Expr
readExpr input = case parse exprParser input of
  Just (expr, "") -> Just expr
  _               -> Nothing



