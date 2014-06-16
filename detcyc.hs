import System.Environment

-- | 
-- >>> detcyc [2,0,6,3,1,6,3,1,6,3,1]
-- [6,3,1]
--
-- >>> detcyc [3,4,8,0,11,9,7,2,5,6,10,1,49,49,49,49]
-- [49]
--
-- >>> detcyc [1,2,3,1,2,3,1,2,3]
-- [1,2,3]
--
detcyc :: [Int] -> [Int]
detcyc [] = []
detcyc (x:xs) = if null r then detcyc xs else x:l where
  (l,r) = span (/=x) xs

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (putStrLn . format . detcyc . parse) strs where
    parse str = map (\x -> read x :: Int) $ words str
    format xs = unwords $ map (\x -> show x) xs

