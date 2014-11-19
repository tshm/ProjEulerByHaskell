import System.Environment (getArgs)
import Text.Printf

-- | get ratio of lowercase and uppercase letters
-- >>> caseRatio "AAbcBd"
-- (50.0,50.0)
--
caseRatio :: String -> (Double, Double)
caseRatio str = (lss, uss) where
  tot = (fromIntegral $ length str) :: Double
  (ls, us) = foldl xx (0, 0) str
  xx (l, u) c = (l', u') where
    l' = if c `elem` ['a'..'z'] then l + 1 else l
    u' = if c `elem` ['A'..'Z'] then u + 1 else u
  lss = 100.0 * ls / tot
  uss = 100.0 * us / tot

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . format . caseRatio) $ lines doc where
    format (x, y) = printf "lowercase: %.2f uppercase: %.2f" x y

