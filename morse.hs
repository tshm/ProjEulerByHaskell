import System.Environment (getArgs)
import Data.Maybe (fromMaybe)

-- |
-- >>> decl ".- ...- ..--- .-- .... .. . -.-. -..-  ....- ....."
-- "AV2WHIECX 45"
--
-- >>> decl "-... .... ...--"
-- "BH3"
--
-- >>> decl ".-..  ..... ---.. -... .- .... -.-. ----. ---..  . .- -.-  -. .. .-. .--."
--
--
decl :: String -> String
decl xs = map trans codes where
  codes = words $ snd $ foldr repl ('_',"") xs
  repl c (c', s) = if c==' ' && c==c'
                    then (c, " _ " ++ s)
                    else (c, c:s)
  trans code = fromMaybe '_' $ lookup code codeTbl

codeTbl :: [(String, Char)]
codeTbl = [("_", ' '),
  (".----", '1'),
  ("..---", '2'),
  ("...--", '3'),
  ("....-", '4'),
  (".....", '5'),
  ("-....", '6'),
  ("--...", '7'),
  ("---..", '8'),
  ("----.", '9'),
  ("-----", '0'),
  (".-",   'A'), 
  ("-...", 'B'),
  ("-.-.", 'C'),
  ("-..",  'D'),
  (".",    'E'),
  ("..-.", 'F'),
  ("--.",  'G'),
  ("....", 'H'),
  ("..",   'I'),
  (".---", 'J'),
  ("-.-",  'K'),
  (".-..", 'L'),
  ("--",   'M'),
  ("-.",   'N'),
  ("---",  'O'),
  (".--.", 'P'),
  ("--.-", 'Q'),
  (".-.",  'R'),
  ("...",  'S'),
  ("-",    'T'),
  ("..-",  'U'),
  ("...-", 'V'),
  (".--",  'W'),
  ("-..-", 'X'),
  ("-.--", 'Y'),
  ("--..", 'Z')]


main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . decl) $ lines doc

