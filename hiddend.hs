import System.Environment (getArgs)
import Data.List (elemIndex)

-- |
-- >>> decl "abcdefghik3"
-- "0123456783"
--
-- >>> decl "abcdefghik"
-- "012345678"
--
decl :: String -> String
decl xs = if null xs' then "NONE" else xs' where
  xs' = foldr trans "" xs
  trans c s = case elemIndex c "0123456789" of
    Just _  -> c : s
    Nothing -> case elemIndex c "abcdefghij" of
      Just i  -> show i ++ s
      Nothing -> s

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . decl) $ lines doc

