import Data.List
import System.Environment

-- |
-- >>> getLowU $ parse "3 3 9 1 6 5 8 1 5 3"
-- 5
--
-- >>> getLowU $ parse "9 2 9 9 1 8 8 8 2 1 1"
-- 0
--
getLowU :: [Int] -> Int
getLowU xs = if null uniqs then 0 else index where
  uniqs = concat $ filter ((==1) . length) $ group $ sort xs
  index = case elemIndex (head uniqs) xs of
    Just i  -> i + 1
    Nothing -> 0

parse :: String -> [Int]
parse str = map (\xs -> read xs :: Int) $ words str

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (print . getLowU . parse) strs
  
