-- Challenge url: https://www.hackerrank.com/challenges/cats-and-a-mouse/problem

import Control.Monad (replicateM_)

main :: IO ()
main = readLn >>= flip replicateM_ (getLine >>= putStrLn . catMouse . map read . words)

catMouse :: [Int] -> String
catMouse (a : b : c : _)
  | abs (a - c) < abs (b - c) = "Cat A"
  | abs (a - c) > abs (b - c) = "Cat B"
  | otherwise = "Mouse C"
