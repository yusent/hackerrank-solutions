-- Challenge url: https://www.hackerrank.com/challenges/password-cracker-fp/problem

import Control.Monad (replicateM_)
import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    _ <- getLine
    passwords <- removeRedundantPwds . words <$> getLine
    loginAttempt <- getLine
    putStrLn $ maybe "WRONG PASSWORD" unwords $ validate passwords loginAttempt

removeRedundantPwds :: [String] -> [String]
removeRedundantPwds = removeRedundantPwds' . reverse . sortBy (compare `on` length)
  where
    removeRedundantPwds' [] = []
    removeRedundantPwds' (password : passwords)
      | isJust $ validate passwords password = removeRedundantPwds' passwords
      | otherwise = password : removeRedundantPwds' passwords

validate :: [String] -> String -> Maybe [String]
validate _ [] = Just []
validate passwords string = fmap fromJust . find isJust $ split <$> passwords
  where
    split pass = fmap (pass :) . validate passwords =<< removeIfPrefix pass string

removeIfPrefix :: String -> String -> Maybe String
removeIfPrefix [] result = Just result
removeIfPrefix _ [] = Nothing
removeIfPrefix (x : xs) (y : ys)
  | x == y = removeIfPrefix xs ys
  | otherwise = Nothing
