-- Challenge url: https://www.hackerrank.com/challenges/sequence-full-of-colors/problem

import Control.Monad (replicateM_)

main :: IO ()
main = readLn >>= flip replicateM_ (getLine >>= print . colorful)

colorful :: String -> Bool
colorful = colorful' 0 0
  where
    colorful' 0 0 "" = True
    colorful' rg yb (c : string) = case c of
      'R' -> rg > (-1) && colorful' (rg - 1) yb string
      'G' -> rg < 1 && colorful' (rg + 1) yb string
      'Y' -> yb > (-1) && colorful' rg (yb - 1) string
      'B' -> yb < 1 && colorful' rg (yb + 1) string
    colorful' _ _ _ = False
