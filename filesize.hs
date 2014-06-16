import System.Environment
import System.IO

main :: IO ()
main = do
  [filename] <- getArgs
  size <- withBinaryFile filename ReadMode hFileSize
  print size

