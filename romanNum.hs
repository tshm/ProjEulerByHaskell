import System.Environment

-- | Encoding to Roman Numbers
-- >>> toRoman "4"
-- "IV"
--
-- >>> toRoman "8"
-- "VIII"
--
-- >>> toRoman "5"
-- "V"
--
-- >>> toRoman "159"
-- "CLIX"
--
-- >>> toRoman "296"
-- "CCXCVI"
--
-- >>> toRoman "3992"
-- "MMMCMXCII"
--
toRoman :: String -> String
toRoman num = foldr1 (++) $ reverse is where
  zipped = zip ["IVX", "XLC", "CDM", "M__"] $ reverse num
  is = foldr (\([i,v,x],n) s -> conv n i v x : s ) [] zipped
  conv m i v x = case m of
    '0' -> ""
    '1' -> [i]
    '2' -> [i, i]
    '3' -> [i, i, i]
    '4' -> [i, v]
    '5' -> [v]
    '6' -> [v, i]
    '7' -> [v, i, i]
    '8' -> [v, i, i, i]
    '9' -> [i, x]
    _ -> []

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . toRoman) $ lines doc

