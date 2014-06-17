import System.Environment
import Data.List (sortBy)

-- | pick 'n' longest lines from the list
-- >>> longestLines 1 ["test", "test1"]
-- ["test1"]
--
-- >>> longestLines 1 ["xx", "aaa", "yy"]
-- ["aaa"]
--
longestLines :: Int -> [String] -> [String]
longestLines n strs = take n sorted where
  sorted = sortBy (\x y -> length y `compare` length x) strs

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  let inputs = lines doc
  let n = (read $ head inputs) :: Int
  mapM_ putStrLn inputs
  --mapM_ putStrLn $ longestLines n (tail inputs)

