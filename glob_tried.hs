import System.Environment (getArgs)
import Text.ParserCombinators.ReadP
import Control.Monad (foldM,liftM)

-- |
-- >>> globMatch "*7*" ["johab.py", "gen_probe.ko", "palmtx.h", "macpath.py", "tzp", "dm-dirty-log.h", "bh1770.h", "pktloc", "faillog.8.gz", "zconf.gperf"]
-- ["bh1770.h"]
--
-- >>> null $ (readP_to_S $ globParser "*7*") "pktloc"
-- True
--
-- >>> null $ (readP_to_S $ globParser "*7*") "bh1770.h"
-- False
--
globMatch :: String -> [String] -> [String]
globMatch p = filter isMatch where
  parser = globParser p
  isMatch s = any mm $ readP_to_S parser s where
    mm (matched, rest) = null rest && matched == s

-- |
-- >>> null $ (readP_to_S $ globParser "7*b") "882ab"
-- True
--
-- >>> fst . head $ (readP_to_S $ globParser "7*b") "782ab"
-- "782ab"
--
-- >>>  (readP_to_S $ globParser "*.???") "abc.txtdump"
-- "abc.txt"
--
-- >>> fst . head $ (readP_to_S $ globParser "782[abcdefg]b") "782ab"
-- "782ab"
--
globParser :: String -> ReadP String
globParser p = foldM f "" (fst . last $ readP_to_S parseGlob p) where
  f :: String -> GlobElem -> ReadP String
  f s g = liftM (s ++) $ case g of
    Star       -> many $ satisfy (const True)
    Ques       -> satisfy (const True) >>= (\c -> return [c])
    Seq ss     -> string ss
    Choice ss  -> satisfy (`elem` ss) >>= (\c -> return [c])

-- |
-- >>> fst . last $ readP_to_S parseGlob "[as]"
-- [Choice "as"]
--
-- >>> fst . last $ readP_to_S parseGlob "ab*[as].?"
-- [Seq "ab",Star,Choice "as",Seq ".",Ques]
--
data GlobElem = Star
              | Ques
              | Seq String
              | Choice String  deriving (Show)

parseGlob :: ReadP [GlobElem]
parseGlob = many $ choice [parseRange,parseQues,parseStar,parseSeq] where
  parseQues = char '?' >> return Ques
  parseStar = char '*' >> return Star
  parseSeq = liftM Seq areNorm
  parseRange = liftM Choice $ between (char '[') (char ']') areNorm
  areNorm = munch1 (`notElem` "*?[]")

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . format . match) $ lines doc where
    match xs = let (p:fs) = words xs in globMatch p fs
    format xs = if null xs then "-" else unwords xs

