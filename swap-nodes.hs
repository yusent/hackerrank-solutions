import Control.Monad (foldM, foldM_, replicateM)

data Tree = Tree Int Tree Tree | Empty

insert :: Int -> Int -> Int -> Tree -> Tree
insert _ _ _ Empty = Empty
insert v' l r (Tree v tl tr)
  | v' == v = Tree v tl' tr'
  | v' < v = Tree v tl tr
  | otherwise = Tree v (insert v' l r tl) (insert v' l r tr)
  where
    tl' = if l < 0 then Empty else Tree l Empty Empty
    tr' = if r < 0 then Empty else Tree r Empty Empty
  
iot :: Tree -> [Int]
iot Empty = []
iot (Tree v tl tr) = iot tl ++ [v] ++ iot tr

swap :: Int -> Int -> Tree -> Tree
swap _ _ Empty = Empty
swap d k (Tree v tl tr)
  | d `mod` k == 0 = Tree v (swap (d+1) k tr) (swap (d+1) k tl)
  | otherwise = Tree v (swap (d+1) k tl) (swap (d+1) k tr)

main :: IO ()
main = do
  n <- readLn
  tree <- foldM foo (Tree 1 Empty Empty) [1..n]
  t <- readLn
  ks <- replicateM t readLn
  foldM_ swp tree ks
  where
    foo t x = do
      (l : r : _) <- map read . words <$> getLine
      return $ insert x l r t
    swp t' k = do
      let tree = swap 1 k t'
      putStrLn . unwords . map show $ iot tree
      return tree
