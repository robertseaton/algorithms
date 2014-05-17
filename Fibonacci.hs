module Fibonacci
       ( fib,
         fib',
         fib'',
         fib''' )
       where

import Data.Matrix (fromList, getElem)

-- Naive, exponential time Fibonacci implementation. O(golden-ration^n) time.
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n < 0 = error "Negative Fibonacci numbers don't exist!"
  | otherwise = fib (n - 1) + fib (n - 2)

-- Bottom up version of the Fibonacci sequence, O(n) time.
fib' :: Int -> Integer
fib' n = fibs !! n
  where
    fibs = [0, 1] ++ zipWith (+) fibs (tail fibs)

-- The golden ratio.
phi :: Double
phi = (1 + sqrt 5) / 2

-- Closed form version of the Fibonacci sequence, O(lg n) time.
fib'' :: Int -> Integer
fib'' n = round $ (phi ^ n - psi ^ n) / sqrt 5
  where
    psi = 1 - phi

-- Matrix version of the Fibonacci sequence, O(lg n) time.
fib''' :: Int -> Integer
fib''' n = getElem 1 2 $ mtrx ^ n
  where
    mtrx = fromList 2 2 [1, 1, 1, 0]
