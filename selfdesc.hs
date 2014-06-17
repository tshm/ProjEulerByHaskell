import System.Environment

-- | 
-- >>> selfdesc "2020"
-- 1
--
-- >>> selfdesc "22"
-- 0
--
-- >>> selfdesc "1210"
-- 1
--
selfdesc :: String -> Int
selfdesc n = if foldl checkDesc True idxArr then 1 else 0 where 
  arr = map (\x -> read [x] :: Int) n
  idxArr = zip [0..] arr
  checkDesc s (i,x) = s && x == length (filter (==i) arr)

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . selfdesc) $ lines doc

