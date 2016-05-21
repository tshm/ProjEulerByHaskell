import Data.List
import System.Environment

-- |
-- >>> isArm "6"
-- True
--
-- >>> isArm "153"
-- True
--
-- >>> isArm "351"
-- False
--
isArm :: String -> Bool
isArm str = (read str :: Int) == s where
  s = sum $ map (\c -> (read [c] :: Int) ^ n) str
  n = length str

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (print . isArm) strs

