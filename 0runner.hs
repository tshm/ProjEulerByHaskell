-- | test and answer seek runner
import System.Environment
import System.Process

compile :: String -> IO String
compile pname = readProcess "ghc" ["-O", "-Wall", pname ++ ".hs"] ""

run :: String -> IO String
run pname = readProcess pname [] ""

main :: IO ()
main = do
  pname <- (!! 0) `fmap` getArgs
  compile pname
  putStrLn =<< (" >>> answer: "++) `fmap` run pname
  
