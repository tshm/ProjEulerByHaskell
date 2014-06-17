import System.Environment (getArgs)
import Data.Char (toLower)

main :: IO ()
main = do
  [fn] <- getArgs
  input <- readFile fn
  mapM_ putStrLn $
    map (map toLower) (lines input)

