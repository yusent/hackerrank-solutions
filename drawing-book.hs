-- Challenge url: https://www.hackerrank.com/challenges/drawing-book/problem

main :: IO ()
main = print =<< pageCount <$> readLn <*> readLn

pageCount :: Float -> Float -> Int
pageCount n p = min pagesFromStart pagesFromEnd
  where
    pagesFromStart = ceiling $ (p - 1) / 2
    pagesFromEnd = ceiling $ (n - closestOdd) / 2
    closestOdd = if odd (floor p) then p else p + 1
