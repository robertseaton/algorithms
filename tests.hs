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
          , testProperty "pow'" pow_t']
      , testGroup "Sorting"
         [ testProperty "mergesort" mergesort_t ]
      ]

pow_t, pow_t' :: Integer -> Positive Integer -> Bool
pow_t x (Positive n) = pow x n  == x ^ n
pow_t' x (Positive n) = pow x n  == x ^ n

mergesort_t :: [Int] -> Bool
mergesort_t xs = sort xs == mergesort xs
