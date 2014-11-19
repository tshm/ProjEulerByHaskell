import System.Environment (getArgs)

-- |
-- >>> findPath (-1) "#########_##"
-- (9,"#########|##")
--
-- >>> findPath (-1) "########_C##"
-- (9,"########_|##")
--
-- >>> findPath (-1) "##C#########"
-- (2,"##|#########")
--
-- >>> findPath (9) "########_C##"
-- (9,"########_|##")
--
-- >>> findPath (8) "########_C##"
-- (9,"########_\\##")
--
findPath :: Int -> String -> (Int,String)
findPath ix xs = (ix', map chPath xs) where
  (ic, ir) = foldr findPathIx (-1,-1) $ zip [0..] xs
  findPathIx (i,c) (ic',ir')
    | c == 'C'  = (i,ir')
    | c == '_'  = (ic',i)
    | otherwise = (ic',ir')
  dir i j
    | ix == -1  = '|'
    | i == -1   = chCh (ix `compare` j)
    | otherwise = chCh (ix `compare` i)
  chCh x
    | x == LT    = '\\'
    | x == GT    = '/'
    | otherwise  = '|'
  ix' = if ic > -1 then ic else ir
  chPath = chPath' (if ic > -1 then 'C' else '_')
  chPath' c' c = if c == c' then dir ic ir else c

-- |
-- >>> steer == race road
-- True
--
race :: [String] -> [String]
race xxs = snd $ foldl proc (-1,[]) xxs where
  proc (ix,s) xs = (ix', s ++ [xs']) where
    (ix', xs') = findPath ix xs

road :: [String]
road = [
  "#########_##",
  "########C_##",
  "#######_####",
  "######_#C###",
  "#######_C###",
  "#######_####",
  "######C#_###",
  "######C_####",
  "#######_####",
  "#######_####"]

steer :: [String]
steer = [
  "#########|##",
  "########/_##",
  "#######/####",
  "######_#\\###",
  "#######_|###",
  "#######/####",
  "######/#_###",
  "######|_####",
  "#######\\####",
  "#######|####"]

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ putStrLn $ race $ lines doc

