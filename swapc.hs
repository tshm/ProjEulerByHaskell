import Data.Char
import System.Environment

-- |
-- >>> swap "Hello"
-- "hELLO"
--
-- >>> swap "Hello world!"
-- "hELLO WORLD!"
--
-- >>> swap "JavaScript language 1.8"
-- "jAVAsCRIPT LANGUAGE 1.8"
--
-- >>> swap "A letter"
-- "a LETTER"
--
swap :: String -> String
swap = map changeCase where
  changeCase c = if isUpper c
                 then toLower c
                 else toUpper c 

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (putStrLn . swap) strs

