import System.Environment (getArgs)

-- |
-- >>> splitcalc "3413289830 a-bcdefghij"
-- -413289827
--
-- >>> splitcalc "776 a+bc"
-- 83
--
-- >>> splitcalc "12345 a+bcde"
-- 2346
--
-- >>> splitcalc "1232 ab+cd"
-- 44
--
-- >>> splitcalc "90602 a+bcde"
-- 611
--
splitcalc :: String -> Int
splitcalc str = if op == '-' then x - y else x + y where
  (num:expr:_) = words str
  (abc, op:_) = span (`notElem` "+-") expr
  len = length abc
  x = read (take len num) :: Int
  y = read (drop len num) :: Int

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . splitcalc) $ lines doc

