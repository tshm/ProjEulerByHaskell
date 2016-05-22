import System.Environment (getArgs)
import Data.Char (digitToInt)
import Numeric (readInt)

-- | convert zero based num to bin
-- >>> zeroBase2Bin "00 0 0 00 00 0"
-- 9
--
-- >>> zeroBase2Bin "00 0"
-- 1
--
-- >>> zeroBase2Bin "00 0 0 000 00 0000000 0 000"
-- 9208
--
-- >>> zeroBase2Bin "0 000000000 00 00"
-- 3
--
zeroBase2Bin :: String -> Int
zeroBase2Bin = readBinary . consume . splitSequence

-- | readbin
-- >>> readBinary "1010"
-- 10
--
readBinary :: String -> Int
readBinary str = fst x where
  x:_ = readInt 2 (`elem` "01") digitToInt str

-- |
-- >>> splitSequence "00 0 0 000 00 0000000 0 000"
-- (["00","0","00","0"],["0","000","0000000","000"])
--
splitSequence :: String -> ([String],[String])
splitSequence str = foldr stack ([],[]) $ zip [(1::Int)..] $ words str where
  stack (i,w) (xs,ys) = if odd i
                     then (w : xs, ys)
                     else (xs, w : ys)

-- |
-- >>> consume (["00","0"],["0","000"])
-- "1000"
--
-- >>> consume (["00","0","00","0"],["0","000","0000000","000"])
-- "10001111111000"
--
consume :: ([String],[String]) -> String
consume (flags, seqs) = binSeq where
  binSeq = consume' (flags, seqs)
  consume' (f:fs, s:ss) = binSeq' ++ consume' (fs, ss) where
    binSeq' = if f == "0"
              then s
              else map (\_ -> '1') s
  consume' (_, _) = ""

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . show . zeroBase2Bin) $ lines doc

