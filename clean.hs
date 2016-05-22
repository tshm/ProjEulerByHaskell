import System.Environment (getArgs)
import Data.Char (isLetter, toLower)

-- | 
-- >>> clean "(--9Hello----World...--)"
-- "hello world"
--
-- >>> clean "Can 0$9 ---you~"
-- "can you"
--
-- >>> clean "13What213are;11you-123+138doing7"
-- "what are you doing"
--
clean :: String -> String
clean str = unwords $ words str' where
  str' = map clean' str
  clean' c = if isLetter c then toLower c else ' '

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . clean) $ lines doc

