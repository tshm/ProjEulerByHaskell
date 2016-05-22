{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> isValid [1,5,5,6,9,1,4,4,6,2,8,5,3,3,9]
-- 1
--
-- >>> isValid [6,3,6,3,1,8,1,1,2,8,5,7,7,6,5,0]
-- 0
--
isValid :: [Int] -> Int
isValid xs = if 0 == (sum xs'') `mod` 10 then 1 else 0 where
  xs' = map even' $ zip [1::Int ..] $ reverse xs
  even' (i, x) = if even i then x * 2 else x
  xs'' = map (\n -> if n > 9 then n - 9 else n) xs'

-- | parse
-- >>> parse "1234 5678 90"
-- [1,2,3,4,5,6,7,8,9,0]
--
parse :: String -> [Int]
parse str = map (\c -> read [c] :: Int) $ concat $ words str

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . isValid . parse) $ lines doc

