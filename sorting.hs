

-- TODO: A split that calls a length might result in worse complexity.
mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort _ [] = []
mergesort op [a] = [a]
mergesort op a = merge op (mergesort op (fst half)) (mergesort op (snd half))
  where half = splitAt (length a `div` 2) a

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge op (x:xs) (y:ys)
  | x `op` y = y : merge op (x:xs) ys
  | otherwise = x : merge op xs (y:ys)
