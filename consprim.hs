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

primes :: [Int]
primes = 2 : sieve [3,5..] where
  sieve [] = []
  sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

-- |
-- >>> perm 1
-- [[1,2]]
--
-- >>> perm 2
-- [[1,2,3,4],[1,4,3,2]]
--
-- >>> perm 3
-- [[1,6,5,2,3,4],[1,4,3,2,5,6]]
--
-- >>> perm 4
-- [[1,6,5,2,3,4],[1,4,3,2,5,6]]
--
perm :: Int -> [[Int]]
perm 1 = [[1,2]]
perm n = concatMap insert $ perm (n-1) where
  insert xs =
    let
      xs'' = xs ++ [2*n-1, 2*n]
      xxs = mapMaybe conv [1..(2*n-3)]
      ll = isPrime (2*n + 1)
      rr = isPrime (last xs + 2*n -1)
      conv i =
        let
          (l, r) = splitAt i xs
          xs' = l ++ [2*n, 2*n-1] ++ r
          lcond = isPrime $ last l + 2*n
          rcond = isPrime $ head r + 2*n - 1
        in if lcond && rcond then Just xs' else Nothing
    in if ll && rr then xs'' : xxs else xxs
  
-- perm :: Int -> [[Int]]
-- perm n = map (1:) $ perm' xe xo where
--   (xe,xo) = partition even [2..n]
--   perm' [] xs = [xs]
--   perm' xs [] = [xs]
--   perm' xs xs' = 
--    let
--      sublist x = map (x:) $ perm' xs' (except x)
--      except x = filter (/=x) xs
--    in foldl1 (++) $ map sublist xs

-- | obviously n has to be odd in order the answer to be non-zero.
-- >>> numconspr 2
-- 1 
--
-- >>> numconspr 4
-- 2 
--
-- >>> numconspr 5
-- 0 
--
-- >>> numconspr 6
-- 2
--
-- >>> numconspr 8
-- 4
--
-- >>> numconspr 10
-- 96
--
numconspr :: Int -> Int
numconspr n = if odd n || n > 18
            then 0
            else length $ perm (div n 2)

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = print -- . numconspr . (read :: String -> Int)


