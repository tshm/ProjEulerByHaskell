import System.Environment
{-
Challenge Description:

Given two integers N and M, calculate N Mod M (without using any inbuilt modulus operator).
Input sample:

Your program should accept as its first argument a path to a filename. Each line in this file contains two comma separated positive integers. E.g.

20,6
2,3

You may assume M will never be zero.
Output sample:

Print out the value of N Mod M 
-}

-- |
-- >>> mymod 20 6
-- 2
--
-- >>> mymod 2 3
-- 2
--
mymod :: Int -> Int -> Int
mymod n m = n - q * m where
  q = n `div` m

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (putStrLn . show . mymodarr . toArr ) strs where
    toArr xs = read $ "[" ++ xs ++ "]"
    mymodarr (n:m:[]) = mymod n m
    mymodarr _ = error "illegal input"

