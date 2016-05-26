import System.Environment (getArgs)
import Data.List (sort)

-- | 
-- >>> parse "**** | *co* | *de* | ****"
-- ((4,4),"*****co**de*****")
--
parse :: String -> ((Int,Int),String)
parse str = ((n,m), str') where
  matrix = words $ map (\c -> if c=='|' then ' ' else c) str
  n = length matrix
  m = length $ head matrix
  str' = concat matrix

-- | 
-- >>> countCodes ["coed","xdec"]
-- 1
--
-- >>> countCodes ["code","odec"]
-- 2
--
countCodes :: [String] -> Int
countCodes strs = length $ filter matches strs where
  matches str = "cdeo" == sort str

-- | 
-- >>> subm (4,4) "*****co**de*****"
-- ["code"]
--
-- >>> subm (2,4) "codxdecx"
-- ["code","odec"]
--
subm :: (Int,Int) -> String -> [String]
subm (n,m) str = filterWord mm where
  mm = [subm' i j | i <- [0..n-2], j <- [0..m-2]]
  filterWord = filter (all (`elem` "code"))
  subm' i j =
    let
      str' = drop (i * m + j) str
      str'' = drop m str'
    in take 2 str' ++ take 2 str''

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . countCodes . uncurry subm . parse) $ lines doc


