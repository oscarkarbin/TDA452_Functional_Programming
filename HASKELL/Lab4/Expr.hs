module Expr where

import Parsing
import Data.Char (isSpace)
import Test.QuickCheck

-- Definition of the Expr data type and related types
-- 'Expr' represents a mathematical expression which can be a number, 
-- an operation (addition or multiplication), a variable 'X', or a function (sin or cos).
data Expr
    = Num Double
    | AddMul Opp Expr Expr
    | X
    | Func SinCos Expr
    deriving (Eq, Show)

-- 'SinCos' defines trigonometric functions: sine and cosine.
data SinCos = Sin | Cos
    deriving (Eq, Show)

-- 'Opp' defines binary operations: addition and multiplication.
data Opp = Add | Mul
    deriving (Eq, Show)

-- Helper functions to create expressions
x :: Expr
x = X

num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add = AddMul Add
mul = AddMul Mul

sin,cos :: Expr -> Expr
sin = Func Sin

cos = Func Cos

-- Example expression
ex1 = AddMul Mul (AddMul Add (Num 1.0) (Func Sin (Num 2))) X

-- Function to calculate the size of an expression
size :: Expr -> Int
size (Num n)          = 0
size X                = 0
size (AddMul _ e1 e2) = 1 + (size e1 + size e2)
size (Func _ e1)      = 1 + size e1

-- Function to convert an expression to a string
showExpr :: Expr -> String
showExpr (Num n)            = show n
showExpr (AddMul Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (AddMul Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2
showExpr (Func Sin e1)    = "sin " ++ showFunc e1
showExpr (Func Cos e1)    = "cos " ++ showFunc e1
showExpr X                  = "x"

-- Function to convert an expression to a string
showFactor :: Expr -> String
showFactor (AddMul Add e1 e2) = "(" ++ showExpr (AddMul Add e1 e2) ++ ")"
showFactor e                  = showExpr e

-- Function to convert an expression to a string
showFunc :: Expr -> String
showFunc (AddMul op e1 e2) = "(" ++ showExpr (AddMul op e1 e2) ++ ")"
showFunc e                = showExpr e

-- Function to evaluate an expression with a given value for 'X'
eval :: Expr -> Double -> Double
eval (Num n) x            = n
eval X x                  = x
eval (AddMul Add e1 e2) x = eval e1 x + eval e2 x
eval (AddMul Mul e1 e2) x = eval e1 x * eval e2 x
eval (Func Sin e1) x      = Prelude.sin (eval e1 x)
eval (Func Cos e1) x      = Prelude.cos (eval e1 x)

-- Parsers for different components of an expression
number :: Parser Expr
number = do
    d <- readsP :: Parser Double
    return $ Num d

parseX :: Parser Expr
parseX = do
           char 'x'
           return X

expr, term, factor :: Parser Expr

expr = foldl1 (AddMul Add) <$> chain term (char '+')
term = foldl1 (AddMul Mul) <$> chain factor (char '*')
factor =
    number
    <|> parseX
    <|> func
    <|> char '(' *> expr <* char ')'


string :: String -> Parser String
string s = filter (not . isSpace) <$> traverse char s

func :: Parser Expr
func = do
    f <- string "sin " <|> string "cos "
    e <- factor
    return $ case f of
        "sin" -> Func Sin e
        "cos" -> Func Cos e


-- Function to parse a string into an 'Expr'
readExpr :: String -> Maybe Expr
readExpr input = case parse expr input of
  Just (expr, "") -> Just expr
  _               -> Nothing

-- Function to generate arbitrary expressions for testing
arbExpr :: Int -> Gen Expr
arbExpr 0 = oneof [return X, Num <$> choose(0.0,100.0)]
arbExpr n | n > 0 = oneof [ Num <$> choose(0.0,100.0)
                          , AddMul <$> elements [Add, Mul] <*> subExpr <*> subExpr
                          , Func <$> elements [Sin, Cos] <*> subExpr
                          ]
  where subExpr = arbExpr (n `div` 2)

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- Function to identify associativity in nested expressions
assoc :: Expr -> Expr
assoc (Num n)                            = Num n
assoc X                                  = X
assoc (AddMul Add (AddMul Add e1 e2) e3) = assoc (AddMul Add e1 (AddMul Add e2 e3))
assoc (AddMul Mul (AddMul Mul e1 e2) e3) = assoc (AddMul Mul e1 (AddMul Mul e2 e3))
assoc (AddMul op e1 e2)                  = AddMul op (assoc e1) (assoc e2)
assoc (Func op e1)                       = Func op (assoc e1)


-- Property to test conversion between 'Expr' and its string representation
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr expr = case readExpr (showExpr expr) of
  Just expr' -> assoc expr == assoc expr'
  Nothing    -> False


-- Simplification functions for addition and multiplication
add' (Num n) (Num m) = Num (n+m)
add' (Num 0) e       = e
add' e       (Num 0) = e
add' e1      e2      = AddMul Add e1 e2

func' :: Expr -> Expr
func' (Func op (Num n)) = Num (eval (Func op (Num n)) 1)
func' (Func op e)       = Func op e

mul' (Num n) (Num m) = Num (n*m)
mul' (Num 0) _       = Num 0
mul' _       (Num 0) = Num 0
mul' (Num 1) e       = e
mul' e       (Num 1) = e
mul' e1      e2      = AddMul Mul e1 e2

-- Function to simplify an expression
simplify :: Expr -> Expr
simplify (AddMul Add e1 e2) = add' (simplify e1) (simplify e2)
simplify (AddMul Mul e1 e2) = mul' (simplify e1) (simplify e2)
simplify (Func op e)        = func' $ Func op (simplify e)
simplify X                  = X
simplify (Num n)            = Num n

-- Property to test if simplification preserves the value of an expression
prop_SimplifyPreservesValue :: Expr -> Double -> Bool
prop_SimplifyPreservesValue expr val = eval expr val == eval (simplify expr) val

-- Function to differentiate an expression
differentiate :: Expr -> Expr
differentiate (Num _)        = Num 0
differentiate X              = Num 1
differentiate (AddMul Add e1 e2) = simplify (AddMul Add (differentiate e1) (differentiate e2))
differentiate (AddMul Mul e1 e2) = simplify (AddMul Add (AddMul Mul (differentiate e1) e2) (AddMul Mul e1 (differentiate e2)))
differentiate (Func Sin e)   = simplify (AddMul Mul (differentiate e) (Func Cos e))
differentiate (Func Cos e)   = simplify (AddMul Mul (Num (-1)) (AddMul Mul (differentiate e) (Func Sin e)))



