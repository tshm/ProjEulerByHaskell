import Data.Char
import System.Environment

-- |
-- >>> myEven 8
-- 1
--
-- >>> myEven 33
-- 0
--
myEven :: Int -> Int
myEven x = if x `mod` 2 == 0 then 1 else 0

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (print . myEven . (\xs -> read xs :: Int)) strs

