import System.Environment (getArgs)
import Data.List

data Time = Time Int Int Int
  deriving (Eq)

instance Show Time where
  show (Time h m s) =
    let
      fmt x = if length str == 2 then str else "0" ++ str where str = show x
    in fmt h ++ ":" ++ fmt m ++ ":" ++ fmt s

-- |
-- >>> Time 14 26 31 < Time 14 26 45
-- True
--
-- >>> Time 02 26 31 < Time 14 44 45
-- True
--
-- >>> Time 22 56 31 < Time 14 44 45
-- False
--
instance Ord Time where
  (<=) (Time h m s) (Time h' m' s') = (h, m, s) <= (h', m', s')

-- |
-- >>> parse "02:26:31 14:44:45 09:53:27"
-- [02:26:31,14:44:45,09:53:27]
--
parse :: String -> [Time]
parse str =
  let
    parseTime xs =
     let 
       [h, m, s] = map (\x -> read x :: Int) $ splitColon xs
       splitColon xs' = words $ map (\c -> if c==':' then ' ' else c) xs'
     in Time h m s
  in map parseTime $ words str

format :: [Time] -> String
format ts = unwords $ map show ts

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . format . sortBy (flip compare) . parse) $ lines doc

