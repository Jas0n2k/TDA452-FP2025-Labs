import Test.QuickCheck
{- Lab 1
   Date: 2025-11-05
   Authors: Adam Jöeäär, Pengfei Li
   Lab group: Group 15
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1


-- B -------------------------
-- power1

power1 :: Integer -> Integer -> Integer
power1 n k = product [n | f <- [1..k]  ]

-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k 
 | even k = power2 (n * n) (k `div` 2)
 | otherwise =  n * power2 n (k - 1)

-- if k is even, we use (n * n) ^ (k `div` 2)
-- if k is odd, we use n * (n ^ (k - 1))

-- D -------------------------
{- 

- (0, k) -> 0 (0 to the power of any positive number is 0)
- (n, 0) -> 1 (Any number to the power of 0 is 1)
- (1, k) -> 1 (1 to the power of any number is 1)
- (2, 4) -> 16 (even k)
- (2, 5) -> 32 (odd k)
- (-3, 2) -> 9 (even k with negative base)
- (-3, 3) -> -27 (odd k with negative base)

 -}

-- 
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k == power1 n k && power n k == power2 n k

--

test_nks :: [(Integer, Integer)] 
test_nks = [(0, 1), (0, 100),(1, 0), (100, 0), (1, 1), (1,100), (2, 4), (2, 5), (-3, 2), (-3, 3)]

powerTest :: Bool
powerTest = and [ prop_powers n k | (n,k) <- test_nks ]

--
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n (abs k)