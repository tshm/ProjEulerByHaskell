import System.Environment (getArgs)

-- Any live cell with fewer than two live neighbors dies, as if caused by under-population.
-- Any live cell with two or three live neighbors lives on to the next generation.
-- Any live cell with more than three live neighbors dies, as if by overcrowding.
-- Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.

-- |
-- >>> format $ stepGame $ parse ["...", "...", "..."]
-- ["...","...","..."]
--
stepGame :: [[ Bool ]]
         -> [[ Bool ]]
stepGame xxs = xxs' where
  xxs' = map stepGame'' [0..n-1]
  stepGame'' i = map (\(x,y) -> stepGame' (x,y) xxs) $ zip (repeat i) mm
  mm = [0..m-1]
  n = length xxs
  m = length $ head xxs

-- |
-- >>> stepGame' (1,1) $ parse ["...", "...", "..."]
-- False
--
stepGame' :: (Int, Int)
         -> [[ Bool ]]
         -> Bool
stepGame' (i, j) xxs
  | live && count < 2           = False
  | live && count `elem` [2,3]  = True
  | live && count > 3           = False
  | not live && count == 3      = True
  | otherwise                   = live
  where
    live = isAlive (i,j) xxs
    count = length $ filter id [isAlive (x,y) xxs | (x,y) <- neighbors (i,j)]
    neighbors (x,y) = map (\(dx,dy) -> (x+dx,y+dy)) idxs
    idxs = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

parse :: [String] -> [[Bool]]
parse = map (map (=='*'))

-- |
-- >>> isAlive (2,2) $ parse ["...", "...", "..*"]
-- True
--
-- >>> isAlive (-1,1) $ parse ["...", ".*.", "..."]
-- False
--
-- >>> isAlive (1,1) $ parse ["...", ".*.", "..."]
-- True
--
isAlive :: (Int, Int)
        -> [[Bool]]
        -> Bool
isAlive (i, j) xxs = inRange && v where
  inRange = i > -1 && j > -1 && i < length xxs && j < length (head xxs)
  v = (xxs !! i) !! j

-- |
-- >>> format $ game 1 $ parse ["...",".*.","..."]
-- ["...","...","..."]
--
game :: Int
     -> [[Bool]]
     -> [[Bool]]
game 0 b = b
game n b = game (n-1) $ stepGame b

format :: [[Bool]] -> [String]
format = map (map (\b -> if b then '*' else '.'))

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ putStrLn $ format $ game 10 $ parse $ lines doc

