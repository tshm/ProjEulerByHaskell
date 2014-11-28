import System.Environment (getArgs)

-- |
-- >>> aromatic "3M1D2C"
-- 3700
--
-- >>> aromatic "3X2I4X"
-- 68
--
-- >>> aromatic "2I3I2X9V1X"
-- -16
--
aromatic :: String -> Int
aromatic [] = 0
aromatic [_] = error "invalid input"
aromatic (x:y:zs)
  | y' >= v'   = aromatic zs + val
  | otherwise  = aromatic zs - val
  where
    val = x' * y'
    rtbl = [('I',1),('V',5),('X',10),('L',50),('C',100),('D',500),('M',1000)]
    x' = read [x] :: Int
    Just y' = lookup y rtbl
    Just v' = if length zs > 1 then lookup (last $ take 2 zs) rtbl else Just 0

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . aromatic) $ lines doc

