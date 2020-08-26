-- Challenge url: https://www.hackerrank.com/challenges/pentagonal-numbers/problem

import Control.Monad (replicateM_)

main :: IO ()
main = readLn >>= flip replicateM_ (readLn >>= print . p)

p :: Int -> Int
p n = (`div` 2) $ n * (n * 3 - 1)
