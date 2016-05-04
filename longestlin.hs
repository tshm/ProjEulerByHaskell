import System.Environment
import Data.List

-- | pick n longest lines
-- >>> longestLines 2 ["x", "xxyyy", "xxx"]
-- ["xxyyy","xxx"]
--
-- >>> longestLines 1 ["x", "xxyyy", "xxx"]
-- ["xxyyy"]
--
longestLines :: Int -> [String] -> [String]
longestLines n strs = ls where
  (ls, _) = foldl pickLonger ([], 0) strs
  pickLonger (buf, minLen) str
    | minLen < length str = (buf', length $ last buf')
    | otherwise           = (buf, minLen)
    where buf' = take n $ sortBy (\x y -> length y `compare` length x) (str:buf)

main :: IO ()
main = do
  [filename] <- getArgs
  (n:strs) <- lines `fmap` readFile filename
  mapM_ putStrLn $ longestLines (read n :: Int) strs

