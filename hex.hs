import Data.List
import System.Environment

-- |
-- >>> hex2dec "f"
-- 15
--
-- >>> hex2dec "9f"
-- 159
--
-- >>> hex2dec "11"
-- 17
--
hex2dec :: String -> Int
hex2dec = foldl (\s h -> s * 16 + oneChar h) 0 where
  mp = '0' : ['1'..'9'] ++ ['a'..'f']
  oneChar h = h' where Just h' = elemIndex h mp


main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (print . hex2dec) strs

