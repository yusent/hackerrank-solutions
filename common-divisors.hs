-- Challenge url: https://www.hackerrank.com/challenges/common-divisors/problem

import Control.Monad (replicateM_)
import Data.List (foldl')
import Data.Set (Set, fromList, size)

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    n : m : _ <- map read . words <$> getLine
    print . (+ 1) . size $ commonDivisors n m

commonDivisors :: Int -> Int -> Set Int
commonDivisors n m = fromList $ commonDivisors' factors
  where
    commonDivisors' (f : fs) =
      let cds = commonDivisors' fs
      in f : foldl' (\acc f' -> f' : f' * f : acc) [] cds
    commonDivisors' _ = []
    factors = commonFactors n m

commonFactors :: Int -> Int -> [Int]
commonFactors n m = commonFactors' (primeFactors n) $ primeFactors m
  where
    commonFactors' (nf : nfs) (mf : mfs)
      | nf == mf = nf : commonFactors' nfs mfs
      | nf > mf = commonFactors' (nf : nfs) mfs
      | otherwise = commonFactors' nfs $ mf : mfs
    commonFactors' _ _ = []

primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n (head primes) (tail primes)
  where
    primeFactors' n prime otherPrimes
      | n < 2 = []
      | n < prime ^ 2 = [n]
      | n `mod` prime == 0 = prime : primeFactors' (n `div` prime) prime otherPrimes
      | otherwise = primeFactors' n (head otherPrimes) (tail otherPrimes)

primes :: [Int]
primes = 2 : 3 : filter (\n-> head (primeFactors n) == n) [5, 7..]
