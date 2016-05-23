import System.Environment (getArgs)
import Text.Regex()
import Text.Regex.Posix

-- | 
-- >>> wine ["Cabernet","Merlot","Noir"] "ot"
-- Just ["Merlot"]
--
-- >>> wine ["Chardonnay","Sauvignon"] "ann"
-- Just ["Chardonnay","Sauvignon"]
--
-- >>> wine ["Shiraz","Grenache"] "o"
-- Nothing
--
wine :: [String] -> String -> Maybe [String]
wine ws pat =
  let
    regex = foldr (\c s -> c : '.' : '*' : s) "" pat
  in case filter (=~ regex) ws of
    [] -> Nothing
    ws' -> Just ws'

-- |
-- >>> parse "Cabernet Merlot Noir | ot"
-- (["Cabernet","Merlot","Noir"],"ot")
--
parse :: String -> ([String], String)
parse str = (ws, pat) where
  (ws, [_, pat]) = break (=="|") $ words str

format :: Maybe [String] -> String
format (Just ws) = unwords ws
format Nothing = "False"

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . format . uncurry wine . parse) $ lines doc
