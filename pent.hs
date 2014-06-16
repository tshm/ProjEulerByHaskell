import System.Environment

-- |
-- >>> pent "some line with text"
-- "with"
--
-- >>> pent "another line"
-- "another"
--
pent :: String -> String
pent = last . init . words

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (putStrLn . pent) strs

