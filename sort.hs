import System.Environment

-- |
-- >>> mysort ["70.920","-38.797","14.354","99.323","90.374","7.581"]
-- ["-38.797","7.581","14.354","70.920","90.374","99.323"]
-- 
-- >>> mysort ["-37.507","-3.263","40.079","27.999","65.213","-55.552"]
-- ["-55.552","-37.507","-3.263","27.999","40.079","65.213"]
--
mysort :: [String] -> [String]
mysort [] = []
mysort (x:xs) = left ++ [x] ++ right where
  left  = mysort [y | y<-xs, readFloat y < readFloat x]
  right = mysort [y | y<-xs, readFloat x < readFloat y]
  readFloat = read :: String -> Float

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (putStrLn . unwords . mysort . words) strs

