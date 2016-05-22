import System.Environment (getArgs)

reverseWords :: String -> String
reverseWords = unwords . reverse . words

main :: IO ()
main = do
  [fn] <- getArgs
  input <- readFile fn
  mapM_ putStrLn $ map reverseWords $ lines input

