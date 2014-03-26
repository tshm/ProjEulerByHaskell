module P22 where
import Text.Regex
import Data.List

-- For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
-- |
-- >>> charScore 'C'
-- 3
--
-- >>> evalScore "COLIN"
-- 53
--
-- >>> loadNames "names.txt" >>= (return . sum . scores)
-- ?

scores xs = map score xss where
  score (i,s) = i * evalScore s
  xss = zip [1..] xs

splitNames :: String -> [String]
splitNames s = init $ tail $ splitRegex sep s where
  sep = mkRegex "\"(,\")?"

loadNames fn = readFile fn >>= (return . sort . splitNames)

evalScore = foldr (\c acc -> charScore c + acc ) 0
charScore c = case elemIndex c ['A'..'Z'] of
  Just n -> n + 1
  Nothing -> 0

--readFile "names.txt" >>= (print . loadNames)

