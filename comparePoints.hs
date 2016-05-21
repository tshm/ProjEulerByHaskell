import System.Environment (getArgs)
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> comparePoints (0, 0) (1, 5)
-- "NE"
--
-- >>> comparePoints (12, 13) (12, 13)
-- "here"
--
-- >>> comparePoints (0, 1) (0, 5)
-- "N"
--
comparePoints :: (Int, Int) -> (Int, Int) -> String
comparePoints (o,p) (q,r)
  |  dx == 0 && dy == 0  = "here"
  |  otherwise = (ns dy) ++ (we dx) where
    ns dy'
      | dy' > 0  = "N"
      | dy' < 0  = "S"
      | otherwise = ""
    we dx'
      | dx' < 0  = "W"
      | dx' > 0  = "E"
      | otherwise = ""
    dx = q - o
    dy = r - p

-- | parsing line
-- >>> parse "0 1 1 5"
-- ((0,1),(1,5))
--
parse :: String -> ((Int, Int), (Int, Int))
parse str = ((o, p), (q, r)) where
  arr = map (read :: String -> Int) $ words str
  (o:p:q:r:_) = arr

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . (uncurry comparePoints) . parse) $ lines doc

