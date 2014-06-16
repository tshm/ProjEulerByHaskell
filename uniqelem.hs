import System.Environment

-- |
-- >>> uniq [1,1,1,2,2,3,4,4,5,5]
-- [1,2,3,4,5]
uniq :: [Int] -> [Int]
uniq [] = []
uniq (x:xs) = x : uniq (dropWhile (x==) xs)

-- |
-- >>> convert "1,1,1,2,2,3,4,4,5,5"
-- "1,2,3,4,5"
convert :: String -> String
convert line = outStr where
  inArr = read $ "[" ++ line ++ "]"
  outStr = tail . init . show . uniq $ inArr

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . convert) $ lines doc

