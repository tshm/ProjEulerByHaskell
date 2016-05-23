import System.Environment (getArgs)
import Data.List (group)
import Data.Char (toLower)

-- | 
-- >>> wine ["Cabernet","Merlot","Noir"] "ot"
-- ["Merlot"]
--
-- >>> wine ["Chardonnay","Sauvignon"] "ann"
-- ["Chardonnay","Sauvignon"]
--
-- >>> wine ["Shiraz","Grenache"] "o"
-- []
--
-- >>> wine ["Shiraz","Grenache"] "ri"
-- ["Shiraz"]
--
wine :: [String] -> String -> [String]
wine ws pat =
  let
    findInWord cs str = all (==[]) cs' where
      cs' = map (\pp -> foldl consum pp str) $ group cs
      consum [] _ = []
      consum ps@(p:ps') c = if p == toLower c then ps' else ps
  in filter (findInWord pat) ws

-- |
-- >>> parse "Cabernet Merlot Noir | ot"
-- (["Cabernet","Merlot","Noir"],"ot")
--
parse :: String -> ([String], String)
parse str = (ws, pat) where
  (ws, [_, pat]) = break (=="|") $ words str

format :: [String] -> String
format [] = "False"
format ws = unwords ws

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . format . uncurry wine . parse) $ lines doc
