import System.Environment (getArgs)
import Data.Maybe(fromMaybe)
import Data.List(elemIndex)

-- |
-- >>> parseNum1 '9'
-- 9
--
-- >>> parseNum1 'X'
-- 0
--
-- >>> parseNum1 'F'
-- 15
--
parseNum1 :: Char -> Int
parseNum1 x = fromMaybe 0 $ elemIndex x "0123456789abcdef"

-- |
-- >>> parseNum 16 "9"
-- 9
--
-- >>> parseNum 16 "FF"
-- 255
--
-- >>> parseNum 2 "11111111"
-- 255
--
parseNum :: Int -> String -> Int
parseNum base = foldl (\s x -> s * base + parseNum1 x) 0

-- |
-- >>> parseLine "64 6e 78 | 100101100 11110"
-- (["64","6e","78"],["100101100","11110"])
--
parseLine :: String -> ([String], [String])
parseLine xs = (hexs, bins) where
  (hexs, _:bins) = break (== "|") $ words xs

-- |
-- >>> isEffective (["64","6e","78"],["100101100","11110"])
-- True
--
-- >>> isEffective (["5e","7d","59"],["1101100","10010101","1100111"])
-- True
--
-- >>> isEffective (["93","75"],["1000111","1011010","1100010"])
-- False
--
isEffective :: ([String], [String]) -> Bool
isEffective (hexs, bins) = v <= av where
  v = foldl (\s x -> s + parseNum 16 x) 0 hexs
  av = foldl (\s x -> s + parseNum 2 x) 0 bins

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . isEffective . parseLine) $ lines doc

