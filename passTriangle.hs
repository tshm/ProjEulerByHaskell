import System.Environment

-- Pass Triangel
-- | propagete the additional cost onto accumulator.
-- >>> propagate [0, 0, 0, 0] [5, 0, 0, 0]
-- [5,0,0,0]
--
-- >>> propagate [5, 0, 0, 0] [9, 6, 0, 0]
-- [14,11,0,0]
--
-- >>> propagate [14, 11, 0, 0] [4, 6, 8, 0]
-- [18,20,19,0]
--
-- >>> propagate [18,20,19,0] [0,7,1,5]
-- [18,27,21,24]
propagate :: [Int] -> [Int] -> [Int]
propagate accum cost = accum' where
  accum' = zipWith3 add accum (0 : accum) cost
  add s0 s1 x = max (s0 + x) (s1 + x)

-- | get max
-- >>> getmax [[5,0,0,0],[9,6,0,0],[4,6,8,0],[0,7,1,5]]
-- 27
getmax :: [[Int]] -> Int
getmax xss = maximum $ foldl1 propagate xss

toInt :: String -> Int
toInt s = read s :: Int

zeroPad :: Int -> [Int] -> [Int]
zeroPad len xs = take len $ xs ++ repeat 0

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  let triangle = map (map toInt . words) $ lines doc
  let rectangle = map (zeroPad (length triangle)) triangle
  putStr $ show $ getmax rectangle

