import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  let nums = map (read :: String -> Int) $ lines doc
  print $ sum nums

