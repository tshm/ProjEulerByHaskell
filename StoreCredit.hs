import System.IO
import Data.List

proc :: String -> String
proc doc = intercalate "\n" $ parse xs where
  (_:xs) = lines doc
  parse :: [String] -> [String]
  parse [] = []
  parse (strc:_:stri:xs') = (show $ buy c items):(parse xs') where
    c = read strc :: Int
    items = map (\w -> read w :: Int) $ words stri

buy :: Int -> [Int] -> [Int]
buy c items = 

main :: IO ()
main = interact proc
  
