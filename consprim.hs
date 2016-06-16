import System.Environment (getArgs)
--import Data.List (partition, permutations)
import Data.Maybe (mapMaybe)
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
-- >>> perm 6
-- [[1,4,3,2,5,6],[1,6,5,2,3,4]]
--
perm :: Int -> [[Int]]
perm n = perm' 1 [2,4..n] [3,5..n] where
  perm' x [] [] = if isPrime (1+x) then [[x]] else []
  perm' x ys zs = foldl (++) [] $ mapMaybe sublist ys where
    sublist y = if isPrime (x+y)
                then Just $ map (x:) $ perm' y zs (except y ys)
                else Nothing
  except y ys = l ++ r where (l,_:r) = break (==y) ys

-- | obviously n has to be odd in order the answer to be non-zero.
-- >>> all (==0) $ map numconspr [1,3,5,7,9,11,13,15,17,18,19,20]
-- True
--
-- >>> map numconspr [2,4,6,8,10,12,14]
-- [1,2,2,4,96,1024,2880]
--
-- >>> numconspr 12
-- 1024
--
numconspr :: Int -> Int
numconspr n =
  if odd n
  then 0
  else length $ perm n

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = print . numconspr . (read :: String -> Int)

