-- Challenge url: https://www.hackerrank.com/challenges/number-of-binary-search-tree/problem

import Control.Monad (forM_, replicateM)
import Data.Map.Strict (Map, (!), (!?), empty, insert)

main :: IO ()
main = do
  t <- readLn
  ts <- replicateM t readLn
  let solutions = countTrees (maximum ts) 1 empty
  forM_ ts (print . (solutions !))

countTrees :: Integer -> Integer -> Map Integer Integer -> Map Integer Integer
countTrees limit n acc
  | n > limit = acc
  | otherwise = countTrees limit (n + 1) $ insert n (countTrees' n) acc
  where
    countTrees' 0 = 1
    countTrees' 1 = 1
    countTrees' x = case acc !? x of
      Nothing -> countTrees'' 0 $ x - 1
      Just c -> c
      where
        (ld, lm) = x `divMod` 2
        countTrees'' a b
          | lm == 0 && a == ld || a > ld = 0
          | otherwise = ( (if a < ld then 2 else 1)
                          * (countTrees' a * countTrees' b)
                          + countTrees'' (a + 1) (b - 1)
                        ) `mod` 100000007
