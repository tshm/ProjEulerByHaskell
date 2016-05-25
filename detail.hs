import System.Environment (getArgs)

data Cell = X | Y | O deriving (Show, Eq)

-- | 
-- >>> parse "XXYY,X..Y,XX.Y"
-- [[X,X,Y,Y],[X,O,O,Y],[X,X,O,Y]]
--
parse :: String -> [[Cell]]
parse str = map (map translate) strs where
  strs = lines $ map (\c -> if c==',' then '\n' else c) str
  translate 'X' = X
  translate 'Y' = Y
  translate _   = O

-- | 
-- >>> shiftable [X,O,Y]
-- 1
--
-- >>> shiftable [X,Y]
-- 0
--
-- >>> shiftable [X,O,Y,X,O,O,O,Y]
-- 1
--
shiftable :: [Cell] -> Int
shiftable cells = shiftable' 0 cells where
  shiftable' i cs =
    let
      cs' = X : cs
      collide = or $ zipWith (\c c' -> c==Y && c'==X) cells cs'
    in if collide then i else shiftable' (i+1) cs'

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . minimum . map shiftable . parse) $ lines doc

