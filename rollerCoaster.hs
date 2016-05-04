import Data.Char
import System.Environment

-- | convert into roller coaster case.
-- >>> roll "test"
-- "TeSt"
--
-- >>> roll "To be, or not to be: that is the question."
-- "To Be, Or NoT tO bE: tHaT iS tHe QuEsTiOn."
--
-- >>> roll "Whether 'tis nobler in the mind to suffer"
-- "WhEtHeR 'tIs NoBlEr In ThE mInD tO sUfFeR"
--
-- >>> roll "The slings and arrows of outrageous fortune,"
-- "ThE sLiNgS aNd ArRoWs Of OuTrAgEoUs FoRtUnE,"
--
-- >>> roll "Or to take arms against a sea of troubles,"
-- "Or To TaKe ArMs AgAiNsT a SeA oF tRoUbLeS,"
--
-- >>> roll "And by opposing end them? To die: to sleep."
-- "AnD bY oPpOsInG eNd ThEm? To DiE: tO sLeEp."
--
roll :: String -> String
roll str = rsl where
  (_, rsl) = foldl capLo (True, "") str
  capLo (f, s) c = (flip f c, s ++ [caseCh f c])
  flip b c = if elem c $ ['a'..'z'] ++ ['A'..'Z'] then not b else b
  caseCh f c = (if f then toUpper else toLower) c

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . roll) $ lines doc

