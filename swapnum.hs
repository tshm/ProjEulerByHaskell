import System.Environment (getArgs)
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> swapNum "0test3"
-- "3test0"
--
swapNum :: String -> String
swapNum (n0:str) = n1:str' ++ [n0] where
  str' = init str
  n1 = last str
swapNum x = x

-- |
-- >>> swapNumLine "4Always0 5look8 4on9 7the2 4bright8 9side7 3of8 5life5"
-- "0Always4 8look5 9on4 2the7 8bright4 7side9 8of3 5life5"
--
-- >>> swapNumLine "5Nobody5 7expects3 5the4 6Spanish4 9inquisition0"
-- "5Nobody5 3expects7 4the5 4Spanish6 0inquisition9"
--
swapNumLine :: String -> String
swapNumLine line = unwords $ map swapNum $ words line

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . swapNumLine) $ lines doc

