-- Challenge url: https://www.hackerrank.com/challenges/password-cracker-fp/problem

import Control.Monad (replicateM_)

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    _ <- getLine
    passwords <- words <$> getLine
    loginAttempt <- getLine
    putStrLn $ maybe "WRONG PASSWORD" unwords $ validate passwords loginAttempt

validate :: [String] -> String -> Maybe [String]
validate _ [] = Just []
validate passwords string = firstJust $ splitPrefix <$> passwords
  where
    splitPrefix pass = fmap (pass :) . validate passwords =<< removeIfPrefix pass string

removeIfPrefix :: String -> String -> Maybe String
removeIfPrefix [] result = Just result
removeIfPrefix _ [] = Nothing
removeIfPrefix (x : xs) (y : ys)
  | x == y = removeIfPrefix xs ys
  | otherwise = Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing : xs) = firstJust xs
firstJust (Just x : _) = Just x
