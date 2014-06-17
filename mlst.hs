import Data.List
import Data.Maybe
import System.Environment

-- |
-- >>> mlst "abcd" 1
-- Just 'd'
--
-- >>> mlst "abcd" 5
-- Nothing
--
-- >>> mlst "abcd" 4
-- Just 'a'
--
mlst :: [a] -> Int -> Maybe a
mlst ax i = if len < 0
            then Nothing
            else Just (head $ drop len ax) where
  len = length ax - i

parse :: String -> (String, Int)
parse str = (xs, n) where
  elms = words str
  xs = concat $ init elms
  n = (read $ last elms) :: Int

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (putStrLn . (:[])) $ mapMaybe (uncurry mlst . parse) strs

