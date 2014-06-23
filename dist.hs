import Data.Char
import System.Environment

-- |
-- >>> findw "osSE5Gu0Vi8WRq93UvkYZCjaOKeNJfTyH6tzDQbxFm4M1ndXIPh27wBA rLclpg| 3 35 27 62 51 27 46 57 26 10 46 63 57 45 15 43 53"
-- "Stephen King 1947"
--
-- >>> findw "3Kucdq9bfCEgZGF2nwx8UpzQJyHiOm0hoaYP6ST1WM7Nks5XjrR4IltBeDLV vA| 2 26 33 55 34 50 33 61 44 28 46 32 28 30 3 50 34 61 40 7 1 31"
-- "Kyotaro Nishimura 1930"
--
findw :: String -> String
findw xs = name where
  (left, _:_:right) = span (/='|') xs
  keys = map (\x -> (read x :: Int) - 1) $ words right
  name = map (left !!) keys

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (putStrLn . findw) strs

