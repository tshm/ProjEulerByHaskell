import System.Environment (getArgs)
import qualified Data.Map as M (toList,filter,fromListWith)

-- |
-- >>> major [1, 1, 1, 2, 2, 3, 1, 1]
-- "1"
--
-- >>> major [92,19,19,76,19,21,19,85,19,19,19,94,19,19,22,67,83,19,19,54,59,1,19,19]
-- "19"
--
-- >>> major [92,11,30,92,1,11,92,38,92,92,43,92,92,51,92,36,97,92,92,92,43,22,84,92,92]
-- "92"
--
-- >>> major [4,79,89,98,48,42,39,79,55,70,21,39,98,16,96,2,10,24,14,47,0,50,95,20,95,48,50,12,42]
-- "None"
--
major :: [Int] -> String
major xs = if null maj then "None" else show (head maj) where
  freq = M.fromListWith (+) $ xs `zip` repeat 1
  maj = map fst $ M.toList $ M.filter (\v -> v * 2 > length xs) freq

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . major . parse) $ lines doc where
    parse xs = (read :: String -> [Int]) $ "[" ++ xs ++ "]"

