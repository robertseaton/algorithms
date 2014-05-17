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
