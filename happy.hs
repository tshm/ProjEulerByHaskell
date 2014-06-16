import Data.List
import System.Environment

-- | 
-- >>> isHappy 1
-- True
-- >>> isHappy 7
-- True
-- >>> isHappy 22
-- False
--
isHappy :: Int -> Bool
isHappy n = isHappy' n [] where
  m = 5000
  isHappy' k xs
    | k == 1         = True
    | length xs > m  = False
    | k `elem` xs    = False
    | otherwise      = isHappy' (sumOfSqDig k) (k:xs)

-- |
-- >>> sumOfSqDig 22
-- 8
--
sumOfSqDig :: Int -> Int
sumOfSqDig n = sum sqDigits where
  sqDigits = map (\c -> (read [c] :: Int)^(2::Integer)) $ show n

conv :: String -> Int
conv line = if isHappy n then 1 else 0 where
  n = read line :: Int

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . conv) $ lines doc

