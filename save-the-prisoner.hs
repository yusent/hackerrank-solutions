import Control.Monad (replicateM_)

main :: IO ()
main = readLn >>= flip replicateM_ warnId
  where
    warnId = do
      n : m : s : [] <- map read . words <$> getLine
      let prisonerId = (m + s - 1) `mod` n
      print $ if prisonerId == 0 then n else prisonerId
