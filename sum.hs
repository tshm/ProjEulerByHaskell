import System.Environment

-- | get sum of digits
-- >>> sumOfDigits 23
-- 5
--
-- >>> sumOfDigits 9
-- 9
--
sumOfDigits :: Int -> Int
sumOfDigits n = total where
  total = foldl1 (+) intArr
  intArr = map (\x -> read [x] :: Int) $ show n

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . show . sumOfDigits . (read :: String -> Int)) $ lines doc

