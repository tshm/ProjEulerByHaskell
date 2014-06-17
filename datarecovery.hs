import System.Environment (getArgs)
import qualified Data.Text as T
import Data.List

decrypt :: T.Text -> T.Text
decrypt input = T.unwords reindexed where
  [front,end] = T.splitOn (T.pack ";") input
  hints = map (\x -> read (T.unpack x) :: Int) (T.words end)
  hints' = hints ++ filter (\x -> not $ elem x hints) [1..length fragments]
  fragments = T.words front
  reindexed = map snd $
    sortBy (\p1 p2 -> fst p1 `compare` fst p2) $
    zip hints' fragments

main :: IO ()
main = do
  [fn] <- getArgs
  input <- readFile fn
  mapM_ putStrLn $
    map (T.unpack . decrypt)
    (T.lines . T.pack $ input)

