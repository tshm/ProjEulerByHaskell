import System.Environment
import Text.Regex.Posix

-- | Sequence Transformation
-- >>> seqX "1010" "AAAAABBBBAAAA"
-- True
--
-- >>> seqX "00" "AAAAAA"
-- True
--
-- >>> seqX "01001110" "AAAABAAABBBBBBAAAAAAA"
-- True
--
-- >>> seqX "1100110" "BBAABABBA"
-- False
seqX :: String -> String -> Bool
seqX pattern target = target =~ regexpattern where
  regexpattern = "^" ++ (concat $ map patmap pattern) ++ "$"
  patmap c = if c == '0' then "A+" else "(A+|B+)"

-- | line by line mapping using seqX
-- >>> iomap "1010 AAAAABBBBAAAA"
-- "Yes"
iomap :: String -> String
iomap line = if isMatch then "Yes" else "No" where
  [pat, tgt] = words line
  isMatch = seqX pat tgt

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  putStr $ unlines $ map iomap $ lines doc

