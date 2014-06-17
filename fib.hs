import System.Environment
import Data.Array

-- | fibs
-- >>> fibn 5
-- 5
--
-- >>> fibn 12
-- 144
--
-- >>> fibn 50
-- 144
--
fibn :: Integer -> Integer
fibn n = fibSeq 5000 ! n

fibSeq m = r where
  r = array (0,m) [(k, f k) | k <- [0..m]]
  f 0 = 0
  f 1 = 1
  f n = r ! (n-1) + r ! (n-2)

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  let inputs = map (read :: String -> Integer) $ lines doc
  let fibSeq' = fibSeq $ maximum inputs
  mapM_ print . (fibSeq' !) inputs

