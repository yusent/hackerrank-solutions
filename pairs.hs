-- Challenge url: https://www.hackerrank.com/challenges/pairs/problem

import Data.List (sort)

main :: IO ()
main = do
  n : k : _ <- map read . words <$> getLine
  getLine >>= (print . findPairs k 0) . sort . map read . words

findPairs :: Int -> Int -> [Int] -> Int
findPairs _ count [] = count
findPairs k count (x : xs) = findPairs k count' xs
  where
    count' = count + (length . takeWhile (== m) $ dropWhile (< m) xs)
    m = x + k
