-- Challenge url: https://www.hackerrank.com/challenges/functional-programming-the-sums-of-powers/problem

main :: IO ()
main = numberOfWays <$> (read <$> getLine) <*> (read <$> getLine) >>= print

numberOfWays :: Int -> Int -> Int
numberOfWays x n = foo n x root
  where
    root = floor $ fromIntegral x ** (1 / fromIntegral n)

foo :: Int -> Int -> Int -> Int
foo _ 0 _ = 1
foo _ _ 0 = 0
foo n x c
  | p <= x = foo n (x - p) (c - 1) + foo n x (c - 1)
  | otherwise = foo n x (c - 1)
  where
    p = c ^ n
