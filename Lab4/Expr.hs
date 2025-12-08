module Expr where

import GHC.Float (cosDouble, sinDouble)
import Parsing
import Data.Char(isSpace)
import Data.Maybe(fromJust)
import Test.QuickCheck

data Expr
  = Bin Expr String Expr
  | --   | Add Expr Expr
    --   | Mul Expr Expr
    Num Double
  | Var
  | Function String Expr
  deriving Eq

instance Show Expr where
   show = showExpr

-- Bin (Num (-1.0)) "*" (Bin (Function "sin" (Num 10.0)) "*" (Num 0.0))

x :: Expr
x = Var

num :: Double -> Expr
num = Num

add, mul :: Expr -> Expr -> Expr
add (Num 0.0) b = b
add a (Num 0.0) = a
add (Num a) (Num b) = Num (a + b)
add a b = Bin a "+" b

mul (Num 1.0) b = b
mul a (Num 1.0) = a

mul (Num 0.0) b = (Num 0.0)
mul a (Num 0.0) = (Num 0.0)

mul (Num a) (Num b) = Num (a * b)
mul a b = Bin a "*" b

sin, cos :: Expr -> Expr
sin = Function "sin"
cos = Function "cos"

-- | Calculates the number of functions and operators in the given expression
size :: Expr -> Int
size (Bin a _ b) = size a + size b + 1
size (Function _ e) = size e + 1
size _ = 0

-- | Puts parenthesis around the given expression
parenthesize :: Expr -> String
parenthesize expr = "(" ++ showExpr expr ++ ")"

-- | Converts the given expression into a string. Will parenthesize + within multiplication operations or + and * inside functions.
showExpr :: Expr -> String
showExpr Var = "x"
showExpr (Num d) = show d
showExpr (Function name e) = name ++ " " ++ showFactor e
  where
    showFactor (Bin a op b) = parenthesize (Bin a op b) -- Any binary operation inside function makes it parenthesized
    showFactor e = showExpr e
showExpr (Bin a op b) = (showFactor a op) ++ op ++ (showFactor b op)
  where
    showFactor (Bin a "+" b) "*" = parenthesize (Bin a "+" b) -- Addition within multiplication must be parenthesized
    showFactor e _ = showExpr e

-- | Evaluates the given expression with the given value for x.
eval :: Expr -> Double -> Double
eval (Bin a "+" b) x = eval a x + eval b x
eval (Bin a "*" b) x = eval a x * eval b x
eval (Function "cos" a) x = cosDouble $ eval a x
eval (Function "sin" a) x = sinDouble $ eval a x
eval (Num d) _ = d
eval Var x = x

-- | Attempts to parses the given string into an expression, returns Nothing on failure.
readExpr :: String -> Maybe Expr
readExpr a = case c of
  Just (x, r) -> Just x
  _ -> Nothing
  where
    c = parse expr clean
    clean = filter (not . isSpace) a

expr   = foldl1 (\a b -> Bin a "+" b) <$> chain term (char '+')
--functions = factor <|> 
term   = foldl1 (\a b -> Bin a "*" b) <$> chain factor (char '*')
factor = varParser 
  <|> Num <$> numParser 
  <|> funParser "sin" <|> funParser "cos"
  <|> char '(' *> expr <* char ')'

-- exprParser :: Parser Expr
-- exprParser = binParser '*'

varParser :: Parser Expr
varParser = do
  c <- char 'x'
  return Var

funParser :: String -> Parser Expr
funParser f = do
  _ <- sequence $ map char f
  e <- expr
  return $ Function f e

numParser :: Parser Double
numParser = readsP :: Parser Double

-- binParser :: Char -> Parser Expr
-- binParser c = do
--   _ <- exprParser
--   op <- char c
--   b <- exprParser
--   return (Bin a [op] b)

test = "2*cos(2*x)"

-- | Property for validating that reading back a shown expression gives the same expression
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr expr = assocEq (fromJust $ readExpr (showExpr expr)) expr

oke = Bin (Num 56) "*" (Bin Var "*" (Bin (Num 57) "*" (Num 53)))
boke = (Bin (Bin (Bin (Num 56) "*" Var) "*" (Num 57)) "*" (Num 53))
-- 56 * (x * (57 * 53))

-- | Checks if two expressions are equal using association rules
assocEq :: Expr -> Expr -> Bool
assocEq (Bin (Bin a "+" b) "+" c) (Bin a' "+" (Bin b' "+" c')) = (assocEq a a') && (assocEq b b') && (assocEq c c') -- TODO: this can probably be refactored somehow...
assocEq (Bin a "+" (Bin b "+" c)) (Bin (Bin a' "+" b') "+" c') = (assocEq a a') && (assocEq b b') && (assocEq c c') 

assocEq (Bin (Bin a "*" b) "*" c) (Bin a' "*" (Bin b' "*" c')) = (assocEq a a') && (assocEq b b') && (assocEq c c')
assocEq (Bin a "*" (Bin b "*" c)) (Bin (Bin a' "*" b') "*" c') = (assocEq a a') && (assocEq b b') && (assocEq c c')
assocEq a b = a == b

instance Arbitrary Expr where 
  arbitrary = sized arbExpr

-- | Generates an arbitrary expression of the given size
arbExpr :: Int -> Gen Expr
arbExpr s = genExpr 
  where 
    genExpr = frequency [
      (1, rVar), 
      (s, rFunction "cos" s), 
      (s, rFunction "sin" s), 
      (1, rNum), 
      (s, rBinOp s)]
    rNum = Num <$> choose (1.0, 100.0)
    rVar = return Var
    rFunction f s = do
      let s' = (s `div` 2) -- Branch to make next size smaller
      f <- elements ["sin", "cos"] -- Random function
      Function f <$> arbExpr s'
    rBinOp op = do
      let s' = (s `div` 2) -- Branch to make next size smaller
      op <- elements ["+", "*"] -- Random operator
      a <- arbExpr s'
      b <- arbExpr s'
      return (Bin a op b)

-- | Simplifies the given expression (merge addition/multiplication of numbers, simplify operations with identity element) 
simplify :: Expr ->  Expr
simplify (Bin a "*" b) = mul (simplify a) (simplify b)
simplify (Bin a "+" b) = add (simplify a) (simplify b)
simplify a = a

-- | Differentiates the given expression 
differentiate :: Expr -> Expr
differentiate (Bin a "+" b)  = Bin (differentiate a) "+" (differentiate b)
differentiate (Bin a "*" b)  = Bin (Bin (differentiate a) "*" b) "+" (Bin (differentiate b) "*" a) -- Product rule
differentiate (Function f x) = Bin (differentiateOuterFunction f x) "*" $ differentiate x -- Chain rule
differentiate Var            = Num 1.0
differentiate _              = Num 0.0

-- | Differentiates the given function expression at top-level, i.e no chain rule
differentiateOuterFunction :: String -> Expr -> Expr
differentiateOuterFunction "sin" x = Function "cos" x
differentiateOuterFunction "cos" x = Bin (Num (-1.0)) "*" (Function "sin" x)

-- | Property for validating that simplify is sound, i.e simplification gives same value as original expression
prop_simplify_sound :: Expr -> Double -> Bool
prop_simplify_sound e x = eval e x == eval (simplify e) x

-- | Property for validating that simplified functions contain no junk
prop_simplify_noJunk :: Expr -> Bool
prop_simplify_noJunk e = not $ containsJunk (simplify e) 
  where
    containsJunk :: Expr -> Bool
    -- Addition between two numbers should be merged
    containsJunk (Bin (Num _) "+" (Num _))   = True
    containsJunk (Bin (Num _) "*" (Num _))   = True
    -- Multiplication with zero is always 0
    containsJunk (Bin (Num 0.0) "*" _) = True
    containsJunk (Bin _ "*" (Num 0.0)) = True
    -- Multiplication with 1 is always other operand
    containsJunk (Bin (Num 1.0) "*" _) = True
    containsJunk (Bin _ "*" (Num 1.0)) = True
    -- Addition with 0 is always other operand
    containsJunk (Bin (Num 0.0) "+" _) = True
    containsJunk (Bin _ "+" (Num 0.0)) = True
    -- Recursively check if subexpressions contain junk
    containsJunk (Bin a _ b)    = containsJunk a || containsJunk b
    containsJunk (Function _ e) = containsJunk e

    containsJunk _ = False
