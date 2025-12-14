module Expr where

import GHC.Float (cosDouble, sinDouble)
import Parsing
import Data.Char(isSpace)
import Data.Maybe(fromJust)
import Test.QuickCheck

-- | A: Recusive data type Expr
-- | I flattened binary operations and functions

data Expr
  = Num Double
  | Var
  | Add Expr Expr
  | Mul Expr Expr
  | Sin Expr
  | Cos Expr
  deriving Eq

instance Show Expr where
    show = showExpr

x :: Expr
x = Var

num :: Double -> Expr
num = Num

-- Dumb contructors (logics moved to func simplify below)
add, mul :: Expr -> Expr -> Expr
add = Add
mul = Mul

sin, cos :: Expr -> Expr
sin = Sin
cos = Cos

-- Counts the number of functions and operators in the given expression
size :: Expr -> Int
size (Add a b) = size a + size b + 1
size (Mul a b) = size a + size b + 1
size (Sin e) = size e + 1
size (Cos e) = size e + 1
size _ = 0 -- Not operators or functions

-- | B: Converts the given expression into a string.
-- | Will parenthesize + within multiplication operations
-- | or + and * inside functions.

-- Helper that puts parenthesis around the string
-- Removes circular dependency of showExpr
parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

-- Add parenthesis around factors that are additions
showFactor :: Expr -> String
showFactor (Add x y) = parenthesize (showExpr (Add x y))
showFactor e = showExpr e

-- Add parenthesis around function arguments that are binary operations
showFunArg :: Expr -> String
showFunArg e = case e of
  Add _ _  -> parenthesize (showExpr e)
  Mul _ _  -> parenthesize (showExpr e)
  _        -> showExpr e

showExpr :: Expr -> String
showExpr (Num d) = show d
showExpr Var = "x"
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b
showExpr (Sin e) = "sin " ++ showFunArg e
showExpr (Cos e) = "cos " ++ showFunArg e

-- | C: Evaluates the given expression with the given value for x.
eval :: Expr -> Double -> Double
eval (Num n) _            = n
eval Var x                = x -- returns the given x value
eval (Add a b) x          = eval a x + eval b x
eval (Mul a b) x          = eval a x * eval b x
eval (Sin e) x            = sinDouble (eval e x)
eval (Cos e) x            = cosDouble (eval e x)

-- | D: Attempts to parses the given string into an expression.
readExpr :: String -> Maybe Expr
readExpr s = case parse parseSum (filter (not . isSpace) s) of
    Just (expr, "") -> Just expr -- Remaning is empty, successful parse
    _               -> Nothing  -- Failed parse

-- | Parse from lowest precedence to highest (addition -> multiplication -> factors)

-- 1. Addition (Sum)
parseSum :: Parser Expr
parseSum = foldl1 Add <$> chain parseProduct (char '+')

-- `chain` parses a list of `parseProduct` separated by '+' characters. :: Parser [Expr]
-- If no '+' chars, passes down
-- `foldl1 Add` then combines this list into a left-associative addition expression.
-- `<$>` applies the function to the result of the parser.
-- e.g "1 + 2 + 3" -> Parser [1,2,3] -> Add (Add 1 2) 3

-- 2. Multiplication (Product)
parseProduct :: Parser Expr
parseProduct = foldl1 Mul <$> chain parseFactor (char '*')

-- 3. Factors: functions, variables, numbers, parenthesized expressions (in this order)
parseFactor :: Parser Expr
parseFactor = 
        parseSin
    <|> parseCos
    <|> parseNumber
    <|> (char 'x' >> return Var)
    <|> (char '(' *> parseSum <* char ')') -- Parenthesized expression, reset to lowest precedence

-- Helpers

parseNumber :: Parser Expr
parseNumber = Num <$> readsP

-- | Since Parsing.hs does not export a parser for strings,
-- | we borrow it from the course example "ParsingExamples.hs"
string :: String -> Parser String
string ""  = return ""
string (c:s) = do
  char c
  string s
  return (c:s)

parseSin :: Parser Expr
parseSin = do
  string "sin"  -- Consume "sin"
  e <- parseFactor -- Parse the argument
  return (Sin e)

parseCos :: Parser Expr
parseCos = do
  string "cos"
  e <- parseFactor
  return (Cos e)


-- | Normalizes the tree structure
-- | by converting right-associative operations into left
-- | e.g A + (B + C)  ==> (A + B) + C
assoc :: Expr -> Expr
assoc (Add a (Add b c)) = assoc (Add (Add a b) c)
assoc (Add a b)         = Add (assoc a) (assoc b)
assoc (Mul a (Mul b c)) = assoc (Mul (Mul a b) c)
assoc (Mul a b)         = Mul (assoc a) (assoc b)
assoc (Sin e)           = Sin (assoc e)
assoc (Cos e)           = Cos (assoc e)
assoc e                 = e

-- | E: Generates an arbitrary expression of the given size

-- | Property for validating that reading back a shown expression gives the same expression
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = 
  case readExpr (showExpr e) of
    Nothing       -> False -- parsing failed, return false
    -- readExpr is already left-associative so only normalize the input expression
    Just parsed   -> assoc parsed == assoc e

instance Arbitrary Expr where 
  arbitrary = sized arbExpr

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [
    (1, rVar), 
    (1, rNum), 
    (s, rFunction), 
    (s, rBinOp)]
  where 
    s' = s `div` 2 -- Branch to make next size smaller
    rNum = Num <$> choose (1.0, 100.0)
    rVar = return Var
    rFunction = do
      f <- elements [Sin, Cos]
      e <- arbExpr s'
      return $ f e
    rBinOp = do
      op <- elements [Add, Mul]
      a <- arbExpr s'
      b <- arbExpr s'
      return $ op a b

-- | F: Simplifies the given expression 
-- | Merges addition/multiplication of numbers
-- | Calculate functions with constant arguments

simplify :: Expr ->  Expr
simplify (Add a b) = simplifyAdd (simplify a) (simplify b)
simplify (Mul a b) = simplifyMul (simplify a) (simplify b)
simplify (Sin e)   = simplifySin (simplify e)
simplify (Cos e)   = simplifyCos (simplify e)
simplify a         = a

-- Helpers
simplifyAdd :: Expr -> Expr -> Expr
simplifyAdd (Num 0.0) b = b
simplifyAdd a (Num 0.0) = a
simplifyAdd (Num a) (Num b) = Num (a + b)
simplifyAdd a b = Add a b

simplifyMul :: Expr -> Expr -> Expr
simplifyMul (Num 0.0) _ = Num 0.0
simplifyMul _ (Num 0.0) = Num 0.0
simplifyMul (Num 1.0) b = b
simplifyMul a (Num 1.0) = a
simplifyMul (Num a) (Num b) = Num (a * b)
simplifyMul a b = Mul a b

simplifySin :: Expr -> Expr
simplifySin (Num a) = Num (sinDouble a)
simplifySin e       = Sin e

simplifyCos :: Expr -> Expr
simplifyCos (Num a) = Num (cosDouble a)
simplifyCos e       = Cos e


-- | Property for validating that simplify is sound, i.e simplification gives same value as original expression
prop_simplify_sound :: Expr -> Double -> Bool
prop_simplify_sound e x = eval e x == eval (simplify e) x

-- | Property for validating that simplified functions contain no junk
prop_simplify_noJunk :: Expr -> Bool
prop_simplify_noJunk e = not $ containsJunk (simplify e) 
  where
    containsJunk :: Expr -> Bool
    -- Addition Junks
    -- both sides are numbers
    containsJunk (Add (Num _) (Num _))   = True
    -- with zeros
    containsJunk (Add (Num 0.0) _) = True
    containsJunk (Add _ (Num 0.0)) = True

    -- Multiplication Junks
    -- both sides are numbers
    containsJunk (Mul (Num _) (Num _))   = True
    -- with zeros
    containsJunk (Mul (Num 0.0) _) = True
    containsJunk (Mul _ (Num 0.0)) = True
    -- with ones
    containsJunk (Mul (Num 1.0) _) = True
    containsJunk (Mul _ (Num 1.0)) = True

    -- Function Junks: constant arguments
    containsJunk (Sin (Num _)) = True
    containsJunk (Cos (Num _)) = True
    -- Recursively check if subexpressions contain junk
    containsJunk (Add a b)    = containsJunk a || containsJunk b
    containsJunk (Mul a b)    = containsJunk a || containsJunk b
    containsJunk (Sin e)      = containsJunk e
    containsJunk (Cos e)      = containsJunk e
    containsJunk _ = False

-- | G: Differentiates the given expression 
differentiate :: Expr -> Expr
differentiate e = simplify (deriv e)
  where
    deriv :: Expr -> Expr
    deriv (Num _)   = Num 0.0       -- Derivative of constant is 0
    deriv Var       = Num 1.0       -- Derivative of x is 1
    deriv (Add a b) = Add (deriv a) (deriv b)      -- Sum Rule
    deriv (Mul a b) = Add (Mul (deriv a) b) (Mul a (deriv b)) -- Product Rule: (uv)' = u'v + uv'
    deriv (Sin e)   = Mul (Cos e) (deriv e)                   -- Chain Rule: (sin u)' = cos u * u'
    deriv (Cos e)   = Mul (Num (-1.0)) (Mul (Sin e) (deriv e)) -- Chain Rule: (cos u)' = -sin u * u'
