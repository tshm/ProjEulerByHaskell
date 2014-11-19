import System.Environment (getArgs)
import Text.Printf

-- | parse time
-- >>> parseTime "14:01:57"
-- 50517
--
parseTime :: String -> Int
parseTime str = sec where
  (hs:ms:ss:_) = map (read :: String -> Int) $ words $ map separate str
  separate c = if c == ':' then ' ' else c
  sec = hs * 60 * 60 + ms * 60 + ss

-- | format time
-- >>> fmtTime 50517
-- "14:01:57"
--
fmtTime :: Int -> String
fmtTime sec = time where
  (hs, x) = sec `divMod` 3600
  (ms, ss) = x `divMod` 60
  time = printf "%02d:%02d:%02d" hs ms ss

-- | diff
-- >>> diffTime "14:01:57 12:47:11"
-- "01:14:46"
--
-- >>> diffTime "13:09:42 22:16:15"
-- "09:06:33"
--
-- >>> diffTime "08:08:06 08:38:28"
-- "00:30:22"
--
-- >>> diffTime "23:35:07 02:49:59"
-- "20:45:08"
--
-- >>> diffTime "14:31:45 14:46:56"
-- "00:15:11"
--
diffTime :: String -> String
diffTime str = diff where
  (start:end:_) = map parseTime $ words str
  diff = fmtTime $ abs $ end - start

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . diffTime) $ lines doc

