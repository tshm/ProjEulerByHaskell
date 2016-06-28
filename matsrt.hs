import System.Environment (getArgs)
import Data.List (sortBy,transpose,intercalate)
--import Data.Maybe (mapMaybe)
--import Debug.Trace (trace)
--import Control.Applicative (liftA2)

-- |
-- >>> matsrt [[1],[0]]
-- [[0],[1]]
--
-- >>> matsrt [[1,3,5],[1,3,2],[1,2,5]]
-- [[1,2,5],[1,3,2],[1,3,5]]
--
matsrt :: [[Int]] -> [[Int]]
matsrt = sortBy matcmp where
  matcmp [] _ = EQ
  matcmp _ [] = EQ
  matcmp (x:xs) (y:ys)
    | x == y     = matcmp xs ys
    | otherwise  = x `compare` y

-- |
-- >>> parse "-3 29 -3 | -17 69 -17 | 44 3 8"
-- [[-3,-17,44],[29,69,3],[-3,-17,8]]
--
parse :: String -> [[Int]]
parse str =
  let
    str' = map (\c -> if c == '|' then '\n' else c) str
    parse' s = map (read :: String -> Int) $ words s
  in transpose $ map parse' $ lines str'

-- |
-- >>> format [[-3,-17,44],[29,69,3],[-3,-17,8]]
-- "-3 -17 44 | 29 69 3 | -3 -17 8"
--
format :: [[Int]] -> String
format xss =
  let
    formatLine xs = unwords $ map show xs
  in intercalate " | " $ map formatLine $ transpose xss

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = putStrLn . format . matsrt . parse

