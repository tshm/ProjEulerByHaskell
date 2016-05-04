import System.Environment (getArgs)
import Data.Char
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> mask "hello" "11001"
-- "HEllO"
--
-- >>> mask "world" "10000"
-- "World"
--
-- >>> mask "cba" "111"
-- "CBA"
--
mask :: String -> String -> String
mask str code = str' where
  str' = zipWith conv str code
  conv ch cd = if cd == '1' then toUpper ch else ch 

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple xs = (head xs, last xs)

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . uncurry mask . toTuple . words) $ lines doc

