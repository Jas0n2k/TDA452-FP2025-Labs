module Expr where

import GHC.Float (cosDouble, sinDouble)
import Parsing
import Data.Char(isSpace)
import Data.Maybe(fromJust)
import Test.QuickCheck

-- | A: Recusive data type Expr

data Expr
  = Bin Expr String Expr
  | Num Double
  | Var
  | Function String Expr
  deriving Eq

instance Show Expr where
    show = showExpr

x :: Expr
x = Var

num :: Double -> Expr
num = Num

-- Dumb contructors (logics moved to func simplify below)
add, mul :: Expr -> Expr -> Expr
add a b = Bin a "+" b
mul a b = Bin a "*" b

sin, cos :: Expr -> Expr
sin = Function "sin"
cos = Function "cos"

-- | Counts the number of functions and operators in the given expression
size :: Expr -> Int
size (Bin a _ b) = size a + size b + 1
size (Function _ e) = size e + 1
size _ = 0 -- Not operators or functions

-- | Puts parenthesis around the given expression
parenthesize :: Expr -> String
parenthesize expr = "(" ++ showExpr expr ++ ")"

-- | B: Converts the given expression into a string.
-- | Will parenthesize + within multiplication operations
-- | or + and * inside functions.
showExpr :: Expr -> String
showExpr Var = "x"
showExpr (Num d) = show d
showExpr (Function name e) = name ++ " " ++ showFactor e
  where
    -- Parenthesize any binary operation within a function
    showFactor (Bin a op b) = parenthesize (Bin a op b) 
    showFactor e = showExpr e
showExpr (Bin a op b) = (showFactor a op) ++ op ++ (showFactor b op)
  where
    -- Parenthesize addition within multiplication
    showFactor (Bin a "+" b) "*" = parenthesize (Bin a "+" b)
    showFactor e _ = showExpr e

-- | C: Evaluates the given expression with the given value for x.
eval :: Expr -> Double -> Double
eval (Bin a "+" b) x      = eval a x + eval b x
eval (Bin a "*" b) x      = eval a x * eval b x
eval (Bin _ op _) _       = error $ "Unsupported binary operator: " ++ op
eval (Function "cos" a) x = cosDouble $ eval a x
eval (Function "sin" a) x = sinDouble $ eval a x
eval (Function f _) _     = error $ "Unsupported function: " ++ f
eval (Num d) _            = d
eval Var x = x  -- returns the given x value

-- | D: Attempts to parses the given string into an expression.
-- |    Returns Nothing on failure.
readExpr :: String -> Maybe Expr
readExpr a = case c of
  Just (x, "")  -> Just x -- Ensure whole input is consumed
  Just (x, _)   -> Just x -- Partial parse, still return result
  _ -> Nothing
  where
    c = parse expr clean
    clean = filter (not . isSpace) a

binOp :: String -> Expr -> Expr -> Expr
binOp op a b = Bin a op b

expr, term, factor :: Parser Expr
expr = foldl1 (binOp "+") <$> chain term (char '+') 
term = foldl1 (binOp "*") <$> chain factor (char '*') -- chain factor instead of app

factor = funParser "sin" <|> funParser "cos" <|> atom
  where atom = varParser <|> Num <$> numParser <|> char '(' *> expr <* char ')'

-- | Parser for variable "x"
varParser :: Parser Expr
varParser = do
  char 'x' >> return Var

-- | Parser for function with given name
funParser :: String -> Parser Expr
funParser f = do
  _ <- sequence $ map char f -- Parse function name char by char
  e <- factor -- Only parse a factor as argument to allow chaining
  -- eg: sin x+1 would be parsed as (sin x) + 1
  return $ Function f e

-- | Parser for numbers (always doubles)
numParser :: Parser Double
numParser = readsP :: Parser Double

-- | Property for validating that reading back a shown expression gives the same expression
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr expr = 
  case readExpr (showExpr expr) of
    Nothing       -> False -- parsing failed, return false
    -- readExpr is already left-associative so only normalize the input expression
    Just parsed   -> assoc parsed == assoc expr

-- | Normalizes the tree structure
-- | by converting right-associative operations (a `op` (b `op` c)) into left ((a `op` b) `op` c)
assoc :: Expr -> Expr
assoc (Bin a "+" (Bin b "+" c)) = assoc (Bin (Bin a "+" b) "+" c)
assoc (Bin a "*" (Bin b "*" c)) = assoc (Bin (Bin a "*" b) "*" c)
assoc (Bin a op b) = Bin (assoc a) op (assoc b)
assoc (Function f e) = Function f (assoc e)
assoc e = e

instance Arbitrary Expr where 
  arbitrary = sized arbExpr

-- | E: Generates an arbitrary expression of the given size
arbExpr :: Int -> Gen Expr
arbExpr s = frequency [
    (1, rVar), 
    (s, rFunction), 
    (1, rNum), 
    (s, rBinOp)]
  where 
    s' = s `div` 2 -- Branch to make next size smaller
    rNum = Num <$> choose (1.0, 100.0)
    rVar = return Var
    rFunction = do
      f <- elements ["sin", "cos"] -- Random function
      Function f <$> arbExpr s'
    rBinOp = do
      op <- elements ["+", "*"] -- Random operator
      a <- arbExpr s'
      b <- arbExpr s'
      return (Bin a op b)

-- | F: Simplifies the given expression 
-- | Merges addition/multiplication of numbers
-- | Simplifies operations with identity element
-- | Evaluates functions that don't depend on x
simplify :: Expr ->  Expr
simplify (Bin a "+" b) = simplifyAdd (simplify a) (simplify b)
  where
    simplifyAdd :: Expr -> Expr -> Expr
    simplifyAdd (Num 0.0) b = b
    simplifyAdd a (Num 0.0) = a
    simplifyAdd (Num a) (Num b) = Num (a + b)
    simplifyAdd a b = Bin a "+" b
simplify (Bin a "*" b) = simplifyMul (simplify a) (simplify b)
  where
    simplifyMul :: Expr -> Expr -> Expr
    simplifyMul (Num 0.0) _ = Num 0.0
    simplifyMul _ (Num 0.0) = Num 0.0
    simplifyMul (Num 1.0) b = b
    simplifyMul a (Num 1.0) = a
    simplifyMul (Num a) (Num b) = Num (a * b)
    simplifyMul a b = Bin a "*" b
simplify (Function f e) = simplifyFun $ Function f (simplify e)
  where
    simplifyFun :: Expr -> Expr
    simplifyFun (Function f (Num a)) = Num $ eval (Function f (Num a)) 0.0
    simplifyFun (Function f e) = Function f e
simplify a = a

-- | G: Differentiates the given expression 
differentiate :: Expr -> Expr
differentiate e = simplify $ dif e
  where 
    dif (Bin a "+" b)  = Bin (dif a) "+" (dif b)
    dif (Bin a "*" b)  = Bin (Bin (dif a) "*" b) "+" (Bin (dif b) "*" a) -- Product rule
    dif (Function f innerX) = Bin (differentiateOuterFunction f innerX) "*" $ dif innerX -- Chain rule
    dif Var            = Num 1.0
    dif _              = Num 0.0

-- | Differentiates the given function expression at top-level, i.e no chain rule
differentiateOuterFunction :: String -> Expr -> Expr
differentiateOuterFunction "sin" x = Function "cos" x
differentiateOuterFunction "cos" x = Bin (Num (-1.0)) "*" (Function "sin" x)
differentiateOuterFunction f _ = error $ "Unsupported function for differentiation: " ++ f

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
    -- Functions with values not dependent on x should be evaluated
    containsJunk (Function f (Num a)) = True
    -- Recursively check if subexpressions contain junk
    containsJunk (Bin a _ b)    = containsJunk a || containsJunk b
    containsJunk (Function _ e) = containsJunk e

    containsJunk _ = False