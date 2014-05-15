{-# OPTIONS_GHC -Wall #-}

import Exponents
import Sorting
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.List (sort)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
      [ testGroup "Exponents"
          [ testProperty "pow" pow_t
          , testProperty "pow'" pow_t' ]
      , testGroup "Sorting"
         [ testProperty "mergesort" mergesort_t
         , testProperty "quicksort" quicksort_t
         , testProperty "insertion sort" insertionsort_t ]
      ]

pow_t, pow_t' :: Integer -> Positive Integer -> Bool
pow_t x (Positive n) = pow x n  == x ^ n
pow_t' x (Positive n) = pow x n  == x ^ n

sort_t :: ([Int] -> [Int]) -> [Int] -> Bool
sort_t sortf xs = sort xs == sortf xs

mergesort_t, quicksort_t, insertionsort_t :: [Int] -> Bool
mergesort_t = sort_t mergesort
quicksort_t = sort_t quicksort
insertionsort_t = sort_t insertionsort
