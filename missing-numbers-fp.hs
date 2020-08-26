-- Challenge url: https://www.hackerrank.com/challenges/missing-numbers-fp/problem

import Data.Map (Map, (!?), empty, filter, insert, toAscList, unionWith)
import Prelude hiding (filter)

main :: IO ()
main = do
  _ <- getLine
  a <- getFreqMap empty . map read . words <$> getLine
  _ <- getLine
  b <- getFreqMap empty . map read . words <$> getLine
  putStrLn . unwords . map (show . fst) . toAscList . filter (> 0) $ unionWith (-) b a

getFreqMap :: Map Int Int -> [Int] -> Map Int Int
getFreqMap m [] = m
getFreqMap m (x : xs) = getFreqMap (insert x f' m) xs
  where
    f' = case m !? x of
           Just f -> f + 1
           Nothing -> 1
