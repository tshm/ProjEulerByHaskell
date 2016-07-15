import System.Environment (getArgs)
import Data.List (find, elemIndices)
import Data.Maybe (fromMaybe)

-- |
-- >>> fnrc "yellow"
-- 'y'
--
-- >>> fnrc "tooth"
-- 'h'
--
fnrc :: String -> Char
fnrc xs = fromMaybe ' ' c where
  c = find iscnt1 xs
  iscnt1 x = 1 == (length $ elemIndices x xs)

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = putStrLn . (:[]) . fnrc

