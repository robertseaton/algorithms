{-# OPTIONS_GHC -Wall #-}

module Exponents
       ( pow'
       , pow
       )
       where

import Test.QuickCheck

sqr :: Integer -> Integer
sqr x = x * x

-- The naive implementation.
pow' :: Integer -> Integer -> Integer
pow' x 0 = 1
pow' x n
  | n < 0 = error "Negative exponent"
  | otherwise = x * pow x (n - 1)

-- Divide and conquer, O(lg n) solution.
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x 1 = x
pow x n
  | n < 0 = error "Negative exponent"
  | even n = sqr $ pow x (n `div` 2)
  | odd n = x * (sqr $ pow x ((n - 1) `div` 2))

--quickCheck ((\x n -> pow x n == x ^ n) :: Integer -> Integer -> Bool)


