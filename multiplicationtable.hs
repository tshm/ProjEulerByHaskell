import qualified Data.Text as T

table :: [[Int]]
table = map (\x -> map (x*) src) src where
  src = [1..12] :: [Int]

format :: [[Int]] -> T.Text
format tbl = T.unlines ls where
  ls = map fmt tbl
  fmt = T.intercalate (T.pack "") .
    map (\x -> T.justifyRight 4 ' ' $ T.pack $ show x)

main :: IO ()
main = putStrLn $ T.unpack $ format table

