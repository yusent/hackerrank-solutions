-- Challenge url: https://www.hackerrank.com/challenges/balanced-brackets/problem

import Control.Monad (replicateM_)

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    s <- getLine
    putStrLn $ if balanced s then "YES" else "NO"

balanced :: String -> Bool
balanced = balanced' []
  where
    balanced' [] [] = True
    balanced' (b : openBrackets) (')' : bs) = b == '(' && balanced' openBrackets bs
    balanced' (b : openBrackets) (']' : bs) = b == '[' && balanced' openBrackets bs
    balanced' (b : openBrackets) ('}' : bs) = b == '{' && balanced' openBrackets bs
    balanced' openBrackets (b : bs) = balanced' (b : openBrackets) bs
    balanced' _ _ = False
