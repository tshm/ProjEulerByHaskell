import System.Environment (getArgs)
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> testing "Heelo Codevval | Hello Codeeval"
-- "Low"
--
-- >>> testing "hELLO cODEEVAL | Hello Codeeval"
-- "Critical"
--
-- >>> testing "Hello Codeeval | Hello Codeeval"
-- "Done"
--
testing :: String -> String
testing str = ev cnt where
  (out, (_:_:model)) = span (/= '|') str
  cnt = bugCount (init out) model
  ev :: Int -> String
  ev x
    | x < 1  = "Done"
    | x < 3  = "Low"
    | x < 5  = "Medium"
    | x < 7  = "High"
    | otherwise = "Critical"

bugCount :: String -> String -> Int
bugCount out model = sum diff where
  diff = zipWith (\x y -> if x == y then 0 else 1) out model

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . testing) $ lines doc

