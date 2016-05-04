import System.Environment (getArgs)
import Data.List (maximumBy)
import Data.Ord (comparing)
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> step "hello"
-- "h *e **l ***l ****o"
-- 
step :: String -> String
step xs = unwords $ zipWith replaceWithStar [0..] xs where
  replaceWithStar i x = (replicate i '*') ++ [x]

-- |
-- >>> findMax "cat dog hello"
-- "hello"
-- 
-- >>> findMax "cat dog tom"
-- "cat"
-- 
findMax :: String -> String
findMax = maximumBy (comparing length) . reverse . words

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . step . findMax) $ lines doc

