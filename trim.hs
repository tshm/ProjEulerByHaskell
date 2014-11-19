import System.Environment (getArgs)

-- | trim
-- >>> trim "Tom exhibited."
-- "Tom exhibited."
--
-- >>> trim "Amy Lawrence was proud and glad, and she tried to make Tom see it in her face - but he wouldn't look."
-- "Amy Lawrence was proud and glad, and... <Read More>"
--
-- >>> trim "Tom was tugging at a button-hole and looking sheepish."
-- "Tom was tugging at a button-hole and looking sheepish."
--
-- >>> trim "Two thousand verses is a great many - very, very great many."
-- "Two thousand verses is a great many -... <Read More>"
--
-- >>> trim "Tom's mouth watered for the apple, but he stuck to his work."
-- "Tom's mouth watered for the apple, but... <Read More>"
--
-- >>> trim "123456789A123456789B123456789C123456789D123456789E12345"
-- "123456789A123456789B123456789C123456789D123456789E12345"
--
-- >>> trim "123456789A123456789B123456789C123456789D123456789E123456"
-- "123456789A123456789B123456789C123456789D... <Read More>"
--
-- >>> trim "123456789A123456789B123456789 123456789 123456789E123456"
-- "123456789A123456789B123456789 123456789... <Read More>"
--
trim :: String -> String
trim str
  | length str <= 55  = str
  | otherwise         = trimmed ++ "... <Read More>" where
    trimmed = trimLastword $ take 40 str

-- |
-- >>> trimLastword "abc def "
-- "abc def"
--
-- >>> trimLastword "abc def ghi"
-- "abc def"
--
-- >>> trimLastword "abcdefghi"
-- "abcdefghi"
--
trimLastword :: String -> String
trimLastword str = trimmed where
  ws = words $ str ++ "x"
  trimmed = if not $ elem ' ' str
            then str
            else unwords $ take (length ws - 1) ws

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . trim) $ lines doc

