-- | test and answer seek runner
import System.Environment
import System.Process
import Control.Applicative

compile :: String -> IO String
compile pname = readProcess "ghc" ["-O", "-Wall", pname ++ ".hs"] ""

run :: String -> IO String
run pname = readProcess pname [] ""

main :: IO ()
main = (!! 0) <$> getArgs >>= \pname ->
  compile pname >>
  (" >>> answer:"++) <$> run pname >>= putStrLn
  
