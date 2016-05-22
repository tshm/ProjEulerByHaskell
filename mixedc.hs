import System.Environment (getArgs)

-- |
-- >>> sepl "8,33,21,0,16,50,37,0,melon,7,apricot,peach,pineapple,17,21"
-- "melon,apricot,peach,pineapple|8,33,21,0,16,50,37,0,7,17,21"
--
-- >>> sepl "24,13,14,43,41"
-- "24,13,14,43,41"
--
sepl :: String -> String
sepl xs = map (\c -> if c == ' ' then ',' else c) ws' where
  ws = words $ map (\c -> if c == ',' then ' ' else c) xs
  (as, ns) = foldr sepl' ([],[]) ws
  sepl' w (a,n) = if any (`elem` ['0'..'9']) w
                  then (a, w:n)
                  else (w:a, n)
  ws' = case (unwords as, unwords ns) of
    ([], ns')  -> ns'
    (as', [])  -> as'
    (as', ns') -> as' ++ "|" ++ ns'


main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . sepl) $ lines doc

