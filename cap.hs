import Data.Char
import System.Environment

-- |
-- >>> cap "Hello world"
-- "Hello World"
--
-- >>> cap "javaScript language"
-- "JavaScript Language"
--
-- >>> cap "a letter"
-- "A Letter"
--
-- >>> cap "1st thing"
-- "1st Thing"
--
cap :: String -> String
cap = unwords . map cap' . words where
  cap' [] = []
  cap' (x:xs) = (toUpper x) : xs

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (putStrLn . cap) strs

