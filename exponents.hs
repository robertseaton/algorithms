import Test.QuickCheck

sqr :: Integer -> Integer
sqr x = x * x

pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x 1 = x
pow x n
  | n < 0 = 0
  | even n = sqr $ pow x (n `div` 2)
  | odd n = x * (sqr $ pow x ((n - 1) `div` 2))

--quickCheck ((\x n -> pow x n == x ^ n) :: Integer -> Integer -> Bool)

powt :: Integer -> Positive Integer -> Bool
powt x (Positive n) = pow x n  == x ^ n
