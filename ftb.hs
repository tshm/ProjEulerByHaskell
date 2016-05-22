import System.Environment (getArgs)
import Data.List (intercalate, sort)
import qualified Data.Map as Map

-- |
-- >> team [[1,2,3,4],[3,1],[4,1]]
-- [(1,[1,2,3]),(2,[1]),(3,[1,2]),(4,[1,3])]
--
-- >> team [[19,11],[19,21,23],[31,39,29]]
-- [(11,[1]),(19,[1,2]),(21,[2]),(23,[2]),(29,[3]),(31,[3]),(39,[3])]
--
team :: [[ Int ]] -> [(Int, [Int])]
team css = Map.toList css' where
  list = concat $ zipWith (\t cs -> map (\c -> (c,[t])) cs) [1..] css
  css' = Map.map sort $ Map.fromListWith (++) list

-- |
-- >>> parse "1 2 3 4 | 3 1 | 4 1"
-- [[1,2,3,4],[3,1],[4,1]]
--
-- >>> parse "19 11 | 19 21 23 | 31 39 29"
-- [[19,11],[19,21,23],[31,39,29]]
--
parse :: String -> [[Int]]
parse str = xxs where
  xs' = splitOn '|' str
  xxs = map (map toInt . words) xs'
  toInt s = read s :: Int

-- |
-- >>> splitOn '|' "1 2 3 4 | 3 1 | 4 1"
-- ["1 2 3 4","3 1","4 1"]
--
splitOn :: Char -> String -> [String]
splitOn = splitOn' [] where
  splitOn' accm _ [] = accm
  splitOn' accm c cs = case break (== c) cs of
    (left, []) -> accm ++ [trim left]
    (left, _:right) -> splitOn' (accm ++ [trim left]) c (trim right)
  trim = trim' . trim' where
    trim' = reverse . dropWhile (== ' ')

-- |
-- >>> format [(1,[1,2,3]), (2,[1,2])]
-- "1:1,2,3; 2:1,2;"
--
-- >>> format [(1, [1,2,3]), (2,[1]), (3,[1,2]), (4,[1,3])]
-- "1:1,2,3; 2:1; 3:1,2; 4:1,3;"
--
-- >>> format [(11,[1]), (19,[1,2]), (21,[2]), (23,[2]), (29,[3]), (31,[3]), (39,[3])]
-- "11:1; 19:1,2; 21:2; 23:2; 29:3; 31:3; 39:3;"
--
format :: [(Int,[Int])] -> String
format pairs = str where
  formatNumList xs = intercalate "," (map show xs)
  formatPair (ix, xs) = show ix ++ ":" ++ formatNumList xs ++ ";"
  str = unwords $ map formatPair pairs

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  --mapM_ (print . parse) $ lines doc
  mapM_ (putStrLn . format . team . parse) $ lines doc

