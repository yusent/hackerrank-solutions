main :: IO ()
main = do
    s0 <- getLine
    s1 <- getLine
    k <- readLn
    putStr $ if foo k s0 s1 then "Yes" else "No"

foo :: Int -> String -> String -> Bool
foo k s0 s1
  | k >= length s0 + length s1 = True
  | otherwise = changesLeft >= 0 && even changesLeft
  where
    changesLeft = k - bar s0 s1
    bar z "" = length z
    bar "" z = length z
    bar z0@(c:cc) z1@(k:kk)
      | c == k = bar cc kk
      | otherwise = length z0 + length z1
