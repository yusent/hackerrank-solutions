-- Challenge url: https://www.hackerrank.com/challenges/captain-prime/problem

import Control.Monad (replicateM_)

main :: IO ()
main = readLn >>= flip replicateM_ (getLine >>= putStrLn . fate)

fate :: String -> String
fate id'
  | hasZeros = "DEAD"
  | left && right = "CENTRAL"
  | left = "LEFT"
  | right = "RIGHT"
  | otherwise = "DEAD"
  where
    hasZeros = '0' `elem` id'
    left = obeysLeftRule id'
    right = obeysRightRule id'

obeysLeftRule :: String -> Bool
obeysLeftRule [] = True
obeysLeftRule current@(_ : next) = isPrime (read current) && obeysLeftRule next

obeysRightRule :: String -> Bool
obeysRightRule [] = True
obeysRightRule current = isPrime (read current) && obeysRightRule (init current)

isPrime :: Int -> Bool
isPrime n = n > 1 && null [x | x <- [2..intSqrt n], n `mod` x == 0]

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral
