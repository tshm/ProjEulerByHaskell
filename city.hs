import System.Environment (getArgs)
import Data.List

-- |
-- >>> trans "Rkbs,5453; Wdqiz,1245; Rwds,3890; Ujma,5589; Tbzmo,1303;"
-- [5453,1245,3890,5589,1303]
--
trans :: String -> [Int]
trans str = map spl $ words str where
  spl xs = read dist :: Int where
    dist = init $ tail $ dropWhile (/=',') xs

-- |
-- >>> travel "Rkbs,5453; Wdqiz,1245; Rwds,3890; Ujma,5589; Tbzmo,1303;"
-- "1245,58,2587,1563,136"
--
-- >>> travel "Vgdfz,70; Mgknxpi,3958; Nsptghk,2626; Wuzp,2559; Jcdwi,3761;"
-- "70,2489,67,1135,197"
--
-- >>> travel "Yvnzjwk,5363; Pkabj,5999; Xznvb,3584; Jfksvx,1240; Inwm,5720;"
-- "1240,2344,1779,357,279"
--
-- >>> travel "Ramytdb,2683; Voclqmb,5236;"
-- "2683,2553"
--
travel :: String -> String
travel str = init . tail . show . fst $ arr where
  arr = foldl diff ([],0) $ sort $ trans str
  diff (s,p) x = (s ++ [x - p], x)

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . travel) $ lines doc

