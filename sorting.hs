module Sorting
       ( mergesort )
       where

-- TODO: A split that calls a length might result in worse complexity.
mergesortBy :: (a -> a -> Ordering) -> [a] -> [a]
mergesortBy _ [] = []
mergesortBy _ [a] = [a]
mergesortBy cmp a = merge cmp (mergesortBy cmp (fst half)) (mergesortBy cmp (snd half))
  where half = splitAt (length a `div` 2) a

merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge _ x [] = x
merge _ [] x = x
merge cmp (x:xs) (y:ys)
  | x `cmp` y == GT = y : merge cmp (x:xs) ys
  | otherwise = x : merge cmp xs (y:ys)

mergesort :: Ord a => [a] -> [a]
mergesort = mergesortBy compare
