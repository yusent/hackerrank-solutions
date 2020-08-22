-- Challenge url: https://www.hackerrank.com/challenges/jumping-bunnies/problem

main :: IO ()
main = getLine >> getLine >>= print . foldr (lcm . read) 1 . words
