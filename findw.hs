import Data.Char
import System.Environment

-- |
-- >>> parse "(1, 2) (3, 4)"
-- ((1,2),(3,4))
--
parse :: String -> ((Int,Int),(Int,Int))
parse str = ((x0,y0),(x1,y1)) where
  filt = map (\c -> if c `elem` ", ()" then ' ' else c) str
  (x0:y0:x1:y1:[]) = map (\xs -> read xs :: Int) $ words filt

-- |
-- >>> dist ((25, 4),(1, -6))
-- 26
--
-- >>> dist ((47, 43),(-25, -11))
-- 90
--
dist :: ((Int,Int),(Int,Int)) -> Int
dist ((x0,y0),(x1,y1)) = round $ sqrt d2 where
  d2 = fromIntegral ((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0)) :: Double

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (print . dist . parse) strs

