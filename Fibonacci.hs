module Fibonacci
       ( fib,
         fib',
         fib'',
         fib''' )
       where

import Data.Matrix (fromList, getElem)

-- Naive, exponential time Fibonacci implementation. O(golden-ratio^n) time.
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

-- Closed form version of the Fibonacci sequence, O(lg n) time.
-- Turns out this is only accurate up to 76.
fib'' :: Int -> Integer
fib'' n
  | n < 0 = error "Negative Fibonacci numbers don't exist!"
  | n < 76 = round $ (phi ^ n) / sqrt 5
  | otherwise = fib''' n
  where
    phi = (1 + sqrt 5) / 2

-- Matrix version of the Fibonacci sequence, O(lg n) time.
fib''' :: Int -> Integer
fib''' 0 = 0
fib''' n
  | n < 0 = error "Negative Fibonacci numbers don't exist!"
  | otherwise = getElem 1 2 $ mtrx ^ n
  where
    mtrx = fromList 2 2 [1, 1, 1, 0]
