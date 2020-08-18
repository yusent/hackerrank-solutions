-- Challenge url: https://www.hackerrank.com/challenges/subset-sum/problem

import Control.Monad (forM_, replicateM)
import Data.List (findIndex, scanl1, sortOn)
import Data.Ord (Down(..))
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  _ <- getLine
  sums <- scanl1 (+) . sortOn Down . map read . words <$> getLine
  numberOfTestCases <- readLn
  testCases <- replicateM numberOfTestCases readLn
  let testSet = S.fromList testCases
      sortedTestCases = S.elems testSet
      initialMap = M.fromSet (const (-1)) testSet
      resultsMap = calcResults initialMap sortedTestCases sums 1
  forM_ testCases $ \t -> print $ resultsMap M.! t

calcResults :: M.Map Int Int -> [Int] -> [Int] -> Int -> M.Map Int Int
calcResults accMap (t:ts) (s:ss) index
  | s >= t = calcResults (M.insert t index accMap) ts (s:ss) index
  | otherwise = calcResults accMap (t:ts) ss $ index + 1
calcResults accMap _ _ _ = accMap
