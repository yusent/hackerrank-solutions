-- Challenge url: https://www.hackerrank.com/challenges/crosswords-101/problem

import Control.Monad (replicateM)
import Data.List (permutations)

type Crossword = [String]
data Orientation = Vertical | Horizontal deriving Eq
data Placeholder = Placeholder
    { orientation :: Orientation
    , index :: Int
    , from :: Int
    , to :: Int
    }
type Match = (Placeholder, String)
type Solution = [Match]

main :: IO ()
main = do
  crossword <- replicateM 10 getLine
  words' <- parseWords <$> getLine
  let solution = head $ solutions (parsePlaceholders crossword) words'
  putStrLn $ renderSolution solution crossword

parseWords :: String -> [String]
parseWords = parseWords' [] ""
  where
    parseWords' parsedWords currentWord [] = currentWord : parsedWords
    parseWords' parsedWords currentWord (';' : rest) = parseWords' (currentWord : parsedWords) "" rest
    parseWords' parsedWords currentWord (c : rest) = parseWords' parsedWords (currentWord ++ [c]) rest

parsePlaceholders :: Crossword -> [Placeholder]
parsePlaceholders c = parseHorizontalPlaceholders 0 c ++ parseVerticalPlaceholders c

solutions :: [Placeholder] -> [String] -> [Solution]
solutions placeholders = filter valid . possibleArrangements placeholders
  where
    valid sol = all (\(p, w) -> placeholderLength p == length w) sol && allCoValid sol
    allCoValid (m : ms) = all (coValid m) ms && allCoValid ms
    allCoValid [] = True

renderSolution :: Solution -> Crossword -> String
renderSolution s = unlines . renderSolution' s
  where
    renderSolution' [] c = c
    renderSolution' ((p, w) : rest) c
      | orientation p == Horizontal = let newRow = updateFrom (from p) w $ next !! index p
                                          next = renderSolution' rest c
                                      in  updateAt (index p) newRow next
      | otherwise = renderVerticalMatch p 0 w $ renderSolution' rest c

renderVerticalMatch :: Placeholder -> Int -> String -> Crossword -> Crossword
renderVerticalMatch p i [] rows = rows
renderVerticalMatch p i (x : xs) (row : rows)
  | from p == i = updateAt (index p) x row : renderVerticalMatch p i xs rows
  | otherwise = row : renderVerticalMatch p (i + 1) (x : xs) rows

parseHorizontalPlaceholders :: Int -> Crossword -> [Placeholder]
parseHorizontalPlaceholders _ [] = []
parseHorizontalPlaceholders index (row : rows) =
  parseRow index 0 row ++ parseHorizontalPlaceholders (index + 1) rows

parseRow :: Int -> Int -> String -> [Placeholder]
parseRow _ _ [] = []
parseRow rowIndex colIndex ('+' : rest) = parseRow rowIndex (colIndex + 1) rest
parseRow rowIndex colIndex string
  | wordLength > 1 = Placeholder Horizontal rowIndex colIndex (colIndex + wordLength - 1)
                   : parseRow rowIndex (colIndex + wordLength) rest
  | otherwise = parseRow rowIndex (colIndex + 1) rest
  where
    wordLength = length dashes
    (dashes, rest) = span (== '-') string

parseVerticalPlaceholders :: Crossword -> [Placeholder]
parseVerticalPlaceholders = invertPlaceholders . parseHorizontalPlaceholders 0 . transpose

invertPlaceholders :: [Placeholder] -> [Placeholder]
invertPlaceholders = map invert
  where
    invOrientation Horizontal = Vertical
    invOrientation Vertical = Horizontal
    invert placeholder = placeholder { orientation = invOrientation (orientation placeholder) }

transpose :: [[a]] -> [[a]]
transpose rows@((_ : _) : _) = map head rows : transpose (tail <$> rows)
transpose _ = []

placeholderLength :: Placeholder -> Int
placeholderLength (Placeholder _ _ a b) = b - a + 1

possibleArrangements :: [a] -> [b] -> [[(a, b)]]
possibleArrangements xs = map (zip xs) . permutations

coValid :: Match -> Match -> Bool
coValid m0@(p0, _) m1@(p1, _)
  | cross p0 p1 = charAt (index $ fst m0) m1 == charAt (index $ fst m1) m0
  | otherwise = True

charAt :: Int -> Match -> Char
charAt i (p, w) = w !! (i - from p)

cross :: Placeholder -> Placeholder -> Bool
cross (Placeholder o0 i0 a0 b0) (Placeholder o1 i1 a1 b1) =
  o0 /= o1 && a1 <= i0 && i0 <= b1 && a0 <= i1 && i1 <= b0

updateAt :: Int -> a -> [a] -> [a]
updateAt i x xs = p ++ (x : tail s)
  where
    (p, s) = splitAt i xs

updateFrom :: Int -> [a] -> [a] -> [a]
updateFrom i xs ys = p ++ combine xs s
  where
    (p, s) = splitAt i ys
    combine (a : as) (_ : bs) = a : combine as bs
    combine [] bs = bs
