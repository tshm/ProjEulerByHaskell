import System.Environment (getArgs)
import Data.Bits

-- |
-- >>> split "test,aaa"
-- ["test","aaa"]
split :: String -> [String]
split str = words $ map rep str where
  rep ','  = ' '
  rep c    = c

-- |
-- >>> nums ["33","55"]
-- [33,55]
nums :: [String] -> [Int]
nums strs = map (\s -> read s :: Int) strs

testBitCoincidence :: Int -> Int -> Int -> Bool
testBitCoincidence n p1 p2 = (bitAt n p1) == (bitAt n p2) where
  bitAt n p = testBit n (p - 1)

main :: IO ()
main = do
  [fn] <- getArgs
  input <- readFile fn
  mapM_ putStrLn $
    map ((\b -> if b then "true" else "false") . (\(n:p1:p2:[]) -> testBitCoincidence n p1 p2) . nums . split) (lines input)

