import System.Environment (getArgs)

banner :: String -> [String]
banner str = str' where
  base = ["", "", "", "", "", ""]
  str' = foldr appendBanner base $ clean str
  appendBanner c s = s' where
    seq = banners !! (read (c:[]) :: Int)
    s' = zipWith (++) seq s

clean :: String -> String
clean = filter (\c -> c `elem` ['0'..'9'])

banners :: [[String]]
banners = [
  ["-**--",
   "*--*-",
   "*--*-",
   "*--*-",
   "-**--",
   "-----"],
  ["--*--",
   "-**--",
   "--*--",
   "--*--",
   "-***-",
   "-----"],
  ["***--",
   "---*-",
   "-**--",
   "*----",
   "****-",
   "-----"],
  ["***--",
   "---*-",
   "-**--",
   "---*-",
   "***--",
   "-----"],
  ["-*---",
   "*--*-",
   "****-",
   "---*-",
   "---*-",
   "-----"],
  ["****-",
   "*----",
   "***--",
   "---*-",
   "***--",
   "-----"],
  ["-**--",
   "*----",
   "***--",
   "*--*-",
   "-**--",
   "-----"],
  ["****-",
   "---*-",
   "--*--",
   "-*---",
   "-*---",
   "-----"],
  ["-**--",
   "*--*-",
   "-**--",
   "*--*-",
   "-**--",
   "-----"],
  ["-**--",
   "*--*-",
   "-***-",
   "---*-",
   "-**--",
   "-----"]]


-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStr . unlines . banner) $ lines doc

