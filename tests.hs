{-# OPTIONS_GHC -Wall #-}

import Exponents
import Sorting
import Fibonacci
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.List (sort)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
      [ testGroup "Exponents"
          [ testProperty "divide and conquer pow" pow_t
          , testProperty "naive pow" pow_t' ]
      , testGroup "Sorting"
         [ testProperty "mergesort" mergesort_t
         , testProperty "quicksort" quicksort_t
         , testProperty "insertion sort" insertionsort_t
         , testProperty "selection sort" selectionsort_t ]
      ]

pow_t, pow_t' :: Integer -> Positive Integer -> Bool
pow_t x (Positive n) = pow x n  == x ^ n
pow_t' x (Positive n) = pow x n  == x ^ n

sort_t :: ([Int] -> [Int]) -> [Int] -> Bool
sort_t sortf xs = sort xs == sortf xs

mergesort_t, quicksort_t, insertionsort_t, selectionsort_t :: [Int] -> Bool
mergesort_t = sort_t mergesort
quicksort_t = sort_t quicksort
insertionsort_t = sort_t insertionsort
selectionsort_t = sort_t selectionsort

fib_t :: (Int -> Integer) -> Positive Int -> Bool
fib_t f (Positive n) = f n == fib''' n

fib_t', fib_t'', fib_t''' :: Positive Int -> Bool
fib_t' = fib_t fib'
fib_t'' = fib_t fib''
fib_t''' = fib_t fib'''
