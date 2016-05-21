import System.Environment (getArgs)
import Data.List

-- | knightMove
-- >>> knightMove (2,7)
-- [(4,6),(4,8),(1,5),(3,5)]
--
-- >>> knightMove' "g2"
-- "e1 e3 f4 h4"
--
knightMove :: (Int,Int) -> [(Int,Int)]
knightMove (i,j) = [(i+p,j+q) | (p,q) <- mlist,
  i+p > 0 && i+p < 9,
  j+q > 0 && j+q < 9] where
  mlist = [(2,-1),(2,1),(-2,-1),(-2,1),(-1,2),(1,2),(-1,-2),(1,-2)]

knightMove' :: String -> String
knightMove' (x:y) = unwords $ sort $ map coord $ knightMove (i,j+1) where
  i = read y :: Int
  Just j = findIndex (==x) ['a'..'h']

-- |
-- >>> coord (2,7)
-- "g2"
--
coord :: (Int,Int) -> String
coord (i, j) = (['a'..'h'] !! (j-1)) : show i

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . knightMove') $ lines doc

