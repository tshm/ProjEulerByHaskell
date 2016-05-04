import System.Environment (getArgs)
import Data.List
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> countArrows "<--<<--<<"
-- 2
--
-- >>> countArrows "<<>>--><--<<--<<>>>--><"
-- 4
--
-- >>> countArrows "<-->>"
-- 0
--
countArrows :: String -> Int
countArrows xs = (countSubstring arrowl xs) + (countSubstring arrowr xs)

-- |
-- >>> countSubstring "ab" "ysabssaba"
-- 2
--
countSubstring :: String -> String -> Int
countSubstring _ "" = 0
countSubstring ss ys =
  (if (isPrefixOf ss ys) then 1 else 0) + (countSubstring ss $ tail ys)

arrowl :: String
arrowl = "<--<<"
arrowr :: String
arrowr = ">>-->"

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . countArrows) $ lines doc

