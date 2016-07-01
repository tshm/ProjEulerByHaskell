import System.Environment (getArgs)
import Data.List (transpose,intercalate)

data Direction = U | R | L | D deriving (Show, Eq)

-- |
-- >>> parse "RIGHT; 4; 4 0 2 0|0 0 0 8|4 0 2 4|2 4 2 2"
-- (R,[[4,0,2,0],[0,0,0,8],[4,0,2,4],[2,4,2,2]])
--
-- >>> parse "UP; 4; 2 0 2 0|0 2 0 4|2 8 0 8|0 8 0 16"
-- (U,[[2,0,2,0],[0,2,0,4],[2,8,0,8],[0,8,0,16]])
--
parse :: String -> (Direction, [[Int]])
parse str = (dir, xss) where
  (sdir:_:ss) = lines $ map conv str
  conv c = if c == ';' || c == '|' then '\n' else c
  dir = case sdir of
    "RIGHT" -> R
    "UP" -> U
    "LEFT" -> L
    _ -> D
  xss = map (\xs -> map (read :: String -> Int) $ words xs) ss

-- |
-- >>> slide R [[4,0,2,0],[0,0,0,8],[4,0,2,4],[2,4,2,2]]
-- [[0,0,4,2],[0,0,0,8],[0,4,2,4],[0,2,4,4]]
--
-- >>> slide L [[2,0,2,0],[0,2,8,8],[2,0,0,0],[0,4,8,16]]
-- [[4,0,0,0],[2,16,0,0],[2,0,0,0],[4,8,16,0]]
--
-- >>> slide U [[2,0,2,0],[0,2,0,4],[2,8,0,8],[0,8,0,16]]
-- [[4,2,2,4],[0,16,0,8],[0,0,0,16],[0,0,0,0]]
--
slide :: Direction -> [[Int]] -> [[Int]]
slide D xss = transpose . slide R . transpose $ xss
slide U xss = transpose . slide L . transpose $ xss
slide d xss = map (slide1 d) xss

-- |
-- >>> slide1 L [2,0,2,0]
-- [4,0,0,0]
--
-- >>> slide1 R [4,0,2,0]
-- [0,0,4,2]
--
-- >>> slide1 R [4,0,2,2]
-- [0,0,4,4]
--
-- >>> slide1 R [2,4,2,2]
-- [0,2,4,4]
--
-- >>> slide1 L [0,0,2,2]
-- [4,0,0,0]
--
slide1 :: Direction -> [Int] -> [Int]
slide1 R xs = (replicate (length xs - length xs') 0) ++ xs' where
  xs' = map abs $ foldr mv [] xs
  mv 0 ss = ss
  mv x [] = [x]
  mv x (0:ss) = x:ss
  mv x (s:ss) = if s == x then (-s-x):ss else x:s:ss
slide1 _ xs = reverse . slide1 R . reverse $ xs

-- |
-- >>> format [[1,2],[3,4]]
-- "1 2|3 4"
format :: [[Int]] -> String
format xss = intercalate "|" ss where
  ss = map (\xs -> unwords $ map show xs) xss

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = putStrLn . format . uncurry slide . parse

