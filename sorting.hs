module Sorting
       ( mergesortBy )
       where

import Data.Ord (compare)

-- TODO: A split that calls a length might result in worse complexity.
mergesortBy :: (a -> a -> Ordering) -> [a] -> [a]
mergesortBy _ [] = []
mergesortBy cmp [a] = [a]
mergesortBy cmp a = merge cmp (mergesortBy cmp (fst half)) (mergesortBy cmp (snd half))
  where half = splitAt (length a `div` 2) a

merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge cmp (x:xs) (y:ys)
  | x `cmp` y == GT = y : merge cmp (x:xs) ys
  | otherwise = x : merge cmp xs (y:ys)

mergesort :: Ord a => [a] -> [a]
mergesort = mergesortBy compare
