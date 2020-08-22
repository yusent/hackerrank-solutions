-- Challenge url: https://www.hackerrank.com/challenges/extra-long-factorials/problem

main :: IO ()
main = readLn >>= print . factorial

factorial :: Integer -> Integer
factorial n = product [2..n]
