import Test.QuickCheck

sqr :: Integer -> Integer
sqr x = x * x

-- The naive implementation.
pow' :: Integer -> Integer -> Integer
pow' x 0 = 1
pow' x n
  | n < 0 = 0
  | otherwise = x * pow x (n - 1)

-- Divide and conquer, O(lg n) solution.
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x 1 = x
pow x n
  | n < 0 = 0
  | even n = sqr $ pow x (n `div` 2)
  | odd n = x * (sqr $ pow x ((n - 1) `div` 2))

--quickCheck ((\x n -> pow x n == x ^ n) :: Integer -> Integer -> Bool)

test_pow :: Integer -> Positive Integer -> Bool
test_pow x (Positive n) = pow x n  == x ^ n
