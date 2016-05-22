import System.Environment (getArgs)

-- |
-- >>> split "test,aaa"
-- ["test","aaa"]
split :: String -> [String]
split str = words $ map rep str where
  rep ','  = ' '
  rep c    = c

-- |
-- >>> nums ["33","55"]
-- [33,55]
nums :: [String] -> [Int]
nums strs = map (\s -> read s :: Int) strs

-- |
-- >>> multNumber 13 8
-- 16
-- >>> multNumber 17 16
-- 32
multNumber :: Int -> Int -> Int
multNumber x n = head [ y | y <- [n,n+n..], y >= x ]

main :: IO ()
main = do
  [fn] <- getArgs
  input <- readFile fn
  mapM_ print $
    map ((\(a:b:[]) -> multNumber a b) . nums . split) (lines input)

