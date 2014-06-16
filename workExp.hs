import Data.Array (accumArray,elems)
import Data.List
import System.Environment

-- | get the effective time
-- >>> countYear "Feb 2004-Dec 2009"
-- 5
--
-- >>> countYear "Feb 2004-Dec 2009; Sep 2004-Jul 2008"
-- 5
--
-- >>> countYear "Aug 2013-Mar 2014; Apr 2013-Aug 2013; Jun 2014-Aug 2015; Apr 2003-Nov 2004; Apr 2014-Jan 2015"
-- 4
--
-- >>> countYear "Mar 2003-Jul 2003; Nov 2003-Jan 2004; Apr 1999-Nov 1999"
-- 1
--
-- >>> countYear "Apr 1992-Dec 1993; Feb 1996-Sep 1997; Jan 2002-Jun 2002; Sep 2003-Apr 2004; Feb 2010-Nov 2011"
-- 6
--
-- >>> countYear "Feb 2004-May 2004; Jun 2004-Jul 2004"
-- 0
--
countYear :: String -> Int
countYear line = totalCount `div` 12 where
  linearize (y,m) = 12 * y + m
  maxMonth = last sorted
  minMonth = head sorted
  sorted = sort $ concatMap (\(s,e)->[s,e]) ranges
  totalCount = length $ filter id $ elems workMonths
  workMonths = accumArray (||) False (minMonth,maxMonth) [(i, True) |
    range <- ranges,
    i <- [(fst range)..(snd range)]]
  ranges = map (\(s,e) -> (linearize s, linearize e)) $ split line

-- | parse input line into pair of dates
-- >>> split "Sep 2004-Jul 2008"
-- [((2004,8),(2008,6))]
--
-- >>> split "Feb 2004-Dec 2009; Sep 2004-Jul 2008"
-- [((2004,1),(2009,11)),((2004,8),(2008,6))]
split :: String -> [((Int,Int),(Int,Int))]
split str = map parseSpan spans where
  spans = lines $ map (\c -> if c==';' then '\n' else c) str
  parseSpan line = (parse startStr, parse endStr) where
    line' = if head line == ' ' then tail line else line
    (startStr,_:endStr) = span (/='-') line' 
    parse dateStr = (read y :: Int, fromEnum (read m :: Month)) where
      (m,_:y) = span (/=' ') dateStr

data Month = Jan | Feb | Mar | Apr
           | May | Jun | Jul | Aug
           | Sep | Oct | Nov | Dec deriving (Show, Enum, Read)

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . countYear) $ lines doc

