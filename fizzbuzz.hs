import System.Environment (getArgs)

-- | fizzbuzz
-- >>> fizzbuzzSeq 3 5 10
-- [1 2 F 4 B F 7 8 F B]
-- >> fizzbuzzSeq 2 7 15

fizzbuzzSeq :: Int -> Int -> Int -> [String]
fizzbuzzSeq a b n = map fizzbuzz [1..n] where
  fizzbuzz k
    | fizz && buzz  = "FB"
    | fizz          = "F"
    | buzz          = "B"
    | otherwise     = show k where
      fizz = k `mod` a == 0
      buzz = k `mod` b == 0

main :: IO ()
main = do
  [fn] <- getArgs
  input <- readFile fn
  mapM_ pp $ map (getFizzBuzzSeq . getInts . words) $ lines input where
    getInts = map (\x -> read x :: Int)
    getFizzBuzzSeq (a:b:n:[]) = fizzbuzzSeq a b n
    pp xs = putStrLn $ foldr1 (\x s -> x ++ " " ++ s) xs

