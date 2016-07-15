--import Data.Word
import qualified Data.Word as W
import qualified Data.Binary as B
import Data.Binary.Put

littleEndian :: Bool
littleEndian =  (B.decode $ runPut $ putWord16host 42 :: W.Word8) == 42

-- | entry point
main :: IO ()
main = putStrLn $ if littleEndian then "LittleEndian" else "BigEndian"

