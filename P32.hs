module Main where
import Data.List

unusuals :: [Int]
unusuals = ans where
  ans = nub $ concatMap extractUnusual perms
  perms = permutations [1..9]

-- | separate 9 digit seq into either 1,4 or 2,3 indexes and
-- return list unusual numbers if any.
-- >>> extractUnusual [3,9,1,8,6,7,2,5,4]
-- [7254]
extractUnusual :: [Int] -> [Int]
extractUnusual ds =  (extractUnusual' 1 4 ds) ++ (extractUnusual' 2 3 ds) where
  extractUnusual' f s ds = if (i * j == k) then [k] else [] where
    (i, j, k) = splitAndBuild f s ds

-- | split digit seq into specified indexes and build 3 numbers
-- >>> splitAndBuild 2 3 [3,9,1,8,6,7,2,5,4]
-- (39,186,7254)
splitAndBuild :: Int -> Int -> [Int] -> (Int, Int, Int)
splitAndBuild f s ds = (build first, build second, build third) where
  (ds', third) = splitAt (f+s) ds
  (first, second) = splitAt f ds'

-- | build number out of digit sequence
-- >>> build [1,2,3]
-- 123
build :: [Int] -> Int
build xs = foldl1 (\s x -> s * 10 + x) xs

main :: IO ()
-- main = mapM_ print unusuals
main = do
  print $ sum $ unusuals

