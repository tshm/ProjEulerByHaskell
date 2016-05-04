import System.Environment (getArgs)
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- | vine
-- >>> getVine "Cabernet Merlot Noir | ot"
-- "Merlot"
--
-- >>> getVine "Chardonnay Sauvignon | ann"
-- "Chardonnay Sauvignon"
--
-- >>> getVine "Shiraz Grenache | o"
-- "False"
--
getVine :: String -> String
getVine str = unwords names where
  str 

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  (putStr . getVine) doc

