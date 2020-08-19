-- Challenge url: https://www.hackerrank.com/challenges/jumping-bunnies/problem

main :: IO ()
main = getLine >> getLine >>= print . foldr lcm 1 . map read . words
