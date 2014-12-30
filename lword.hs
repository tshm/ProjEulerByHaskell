import System.Environment (getArgs)
import Data.List
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> lword "some line with text"
-- "some"
--
-- >>> lword "another line"
-- "another"
--
lword :: String -> String
lword str = x where
  x = maximumBy (\u v -> (length u) `compare` (length v)) $ reverse $ words str

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . lword) $ lines doc

