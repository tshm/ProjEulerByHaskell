import System.Environment (getArgs)
import Data.List (intercalate)

-- |
-- >>> takeWhile (< 33) primes
-- [2,3,5,7,11,13,17,19,23,29,31]
--
primes :: [Integer]
primes = sieve [2..] where
  sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

-- |
-- >>> takeWhile (< 20) power2_1
-- [1,3,7,15]
--
-- >>> takeWhile (< 3000) power2_1
-- [1,3,7,15,31]
--
power2_1 :: [Integer]
power2_1 = map (\n -> two ^ n -1 ) [1..]
  where two = 2 :: Integer

-- |
-- >>> intersect [1, 5, 10] [5, 15]
-- [5]
--
-- >>> takeWhile (< 33) merseList
-- [3,7,31]
--
-- >>> getSmallerMerseThan 33
-- [3,7,31]
--
intersect :: [Integer] -> [Integer] -> [Integer]
intersect (x:xs) (y:ys)
  | x == y  = x : intersect xs ys
  | x < y   = xs `intersect` (y:ys)
  | x > y   = (x:xs) `intersect` ys
intersect _ _ = []

merseList :: [Integer]
merseList = primes `intersect` power2_1

toInt :: String -> Integer
toInt str = read str :: Integer

getSmallerMerseThan :: Integer -> [Integer]
getSmallerMerseThan n = takeWhile (< n) merseList

-- |
-- >>> dumpIntList [1, 5, 99]
-- "1, 5, 99"
--
dumpIntList :: [Integer] -> String
dumpIntList xs = intercalate ", " $ map show xs

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . dumpIntList . getSmallerMerseThan . toInt) $ lines doc

