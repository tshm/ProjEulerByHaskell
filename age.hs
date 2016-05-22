import System.Environment (getArgs)

-- | ageGrp
-- >>> ageGrp 5
-- "Elementary school"
--
--
ageGrp :: Int -> String
ageGrp x
  | 0 <= x && x <= 2    = "Still in Mama's arms"
  | 3 <= x && x <= 4    = "Preschool Maniac"
  | 5 <= x && x <= 11   = "Elementary school"
  | 12 <= x && x <= 14  = "Middle school"
  | 15 <= x && x <= 18  = "High school"
  | 19 <= x && x <= 22  = "College"
  | 23 <= x && x <= 65  = "Working for the man"
  | 66 <= x && x <= 100 = "The Golden Years"
  | otherwise           = "This program is for humans"

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . ageGrp . (read :: String -> Int)) $ lines doc

