import Data.List
import Data.Char
import System.Environment

-- |
-- >>> beauty "ABbCcc"
-- 152
--
-- >>> beauty "Good luck in the Facebook Hacker Cup this year!"
-- 754
--
-- >>> beauty "Ignore punctuation, please :)"
-- 491
--
-- >>> beauty "Sometimes test cases are hard to make up."
-- 729
--
-- >>> beauty "So I just go consult Professor Dalves"
-- 646
--
beauty :: String -> Int
beauty str = score where
  cleanStr = map toLower $ filter isAlpha str
  freqs = map length $ group . sort $ cleanStr
  comb = zip [26,25..1] $ sortBy (flip compare) freqs
  score = foldl (\s (v,n) -> s + v * n) 0 comb

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (print . beauty) strs

