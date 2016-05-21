import System.Environment (getArgs)
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> isReal "9999 9999 9999 9999"
-- False
--
-- >>> isReal "9999 9999 9999 9993"
-- True
--
isReal :: String -> Bool
isReal str = (sum nums) `mod` 10 == 0 where
  chars = words $ concatMap (\x -> x : " ") str
  nums = zipWith doubleAtEven [0..] chars
  doubleAtEven :: Int -> String -> Int
  doubleAtEven i x = if even i then y * 2 else y where
    y = read x :: Int

fakeReal :: String -> String
fakeReal str = if real then "Real" else "Fake" where
  real = isReal str

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . fakeReal) $ lines doc

