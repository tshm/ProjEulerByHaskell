import System.Environment (getArgs)
import Data.List (partition)
--import Data.List (partition, permutations)
--import Data.Maybe (mapMaybe)
--import Debug.Trace (trace)
--import Control.Applicative (liftA2)

-- |
-- >>> isPrime 7
-- True
--
isPrime :: Int -> Bool 
isPrime n = n `elem` primes' where
  primes' = takeWhile (< n+1) primes

-- | prime number list for memoize the calculation
primes :: [Int]
primes = 2 : sieve [3,5..] where
  sieve [] = []
  sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

-- |
-- >>> perm 2
-- [[1,2]]
--
-- >>> perm 4
-- [[1,2,3,4],[1,4,3,2]]
--
perm :: Int -> [[Int]]
perm n = map (1:) $ perm' xe xo where
  (xe,xo) = partition even [2..n]
  perm' [] xs = [xs]
  perm' xs [] = [xs]
  perm' xs xs' = 
   let
     sublist x = map (x:) $ perm' xs' (except x)
     except x = filter (/=x) xs
   in foldl1 (++) $ map sublist xs

-- | obviously n has to be odd in order the answer to be non-zero.
-- >>> numconspr 4
-- 2 
--
-- >>> numconspr 5
-- 0 
--
-- >>> numconspr 6
-- 2
--
-- >>> numconspr 12
-- 1024
--
numconspr :: Int -> Int
numconspr n
  | odd n     = 0
  | n > 17    = 0
  | otherwise = length $ filter check (perm n)
    where check xs = all isPrime $ zipWith (+) xs (last xs : xs)

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = print . numconspr . (read :: String -> Int)


