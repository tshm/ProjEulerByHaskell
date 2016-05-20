import System.Environment (getArgs)
import Data.List(elemIndex)
import Data.Maybe(fromMaybe)

-- |
-- >>> rank "A"
-- 12
--
rank :: String -> Int
rank xs = r where
  r = fromMaybe 0 $ elemIndex xs table
  table = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]

-- |
-- >>> pickWinner ("AD", "2H") 'H'
-- Just "2H"
--
-- >>> pickWinner ("KD", "AH") 'C'
-- Just "AH"
--
-- >>> pickWinner ("KD", "KH") 'C'
-- Nothing
--
-- >>> pickWinner ("JH", "10S") 'C'
-- Just "JH"
--
-- >>> pickWinner ("JC", "AC") 'C'
-- Just "AC"
--
pickWinner :: (String, String) -> Char -> Maybe String
pickWinner (c1, c2) t
    | t1 == t2  = highRank
    | t1 == t   = Just c1
    | t2 == t   = Just c2
    | otherwise = highRank
  where
    highRank = if r1 == r2
               then Nothing
               else Just (if r1 > r2 then c1 else c2)
    r1 = rank $ init c1
    r2 = rank $ init c2
    t1 = last c1
    t2 = last c2

-- |
-- >>> parseLine "AD 2H | H"
-- (("AD","2H"),'H')
--
parseLine :: String -> ((String,String),Char)
parseLine str = ((c1, c2), t) where
  (c1:c2:_, _:[t]:_) = break (== "|") $ words str

-- |
-- >>> prettyPickWinner ("AD", "2H") 'H'
-- "2H"
--
-- >>> prettyPickWinner ("KD", "AH") 'C'
-- "AH"
--
-- >>> prettyPickWinner ("KD", "KH") 'C'
-- "KD KH"
--
-- >>> prettyPickWinner ("JH", "10S") 'C'
-- "JH"
--
-- >>> prettyPickWinner ("JC", "AC") 'C'
-- "AC"
--
prettyPickWinner :: ((String, String),Char) -> String
prettyPickWinner ((c1, c2),t)
  = fromMaybe (c1 ++ ' ':c2) $ pickWinner (c1, c2) t

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . prettyPickWinner . parseLine) $ lines doc

