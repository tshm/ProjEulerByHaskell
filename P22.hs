module Main where
import Data.Char
import Data.List

-- For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
-- |
-- >>> charScore 'C'
-- 3
--
-- >>> evalScore "COLIN"
-- 53

scores :: [String] -> [Int]
scores xs = map score xss where
  score (i,s) = i * evalScore s
  xss = zip [1..] xs

splitNames :: String -> [String]
splitNames s = read $ "[" ++ s ++ "]"

loadNames :: FilePath -> IO [String]
loadNames fn = readFile fn >>= (return . sort . splitNames)

evalScore :: String -> Int
evalScore = foldr (\c acc -> charScore c + acc ) 0

charScore :: Char -> Int
charScore c = 1 + ord c - ord 'A'

--readFile "names.txt" >>= (print . loadNames)

main :: IO ()
main = loadNames "names.txt" >>= (print . sum . scores)
