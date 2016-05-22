import System.Environment (getArgs)
import Data.List ()

-- |
-- >>> nclev [4,3,2,1] 1
-- [3,4,2,1]
--
-- >>> nclev [5,4,3,2,1] 2
-- [4,3,5,2,1]
--
nclev :: [ Int ] -> Int -> [ Int ]
nclev xs 0 = xs
nclev xs n = nclev xs' (n-1) where
  xs' = flipNum [] xs
  flipNum :: [Int] -> [Int] -> [Int]
  flipNum left [] = left
  flipNum left [p] = left ++ [p]
  flipNum left (p:q:rest) = if p > q
                          then left ++ (q:p:rest)
                          else flipNum (left ++ [p]) (q : rest)

-- |
-- >>> parse "4 3 2 1 | 1"
-- ([4,3,2,1],1)
--
parse :: String -> ([ Int ], Int)
parse txt = (map toInt left, toInt right) where
  (left, _:right:_) = break (== "|") $ words txt
  toInt str = read str :: Int

-- |
-- >>> format [4,3,2,1]
-- "4 3 2 1"
--
format :: [ Int ] -> String
format xs = unwords xs' where
  xs' = map show xs

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . format . uncurry nclev . parse) $ lines doc

