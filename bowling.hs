import System.Environment (getArgs)
import Control.Monad.State

-- | 
-- >>> score [(8,1)]
--
score = sum . evalState (replicateM 1 (state scoreFrame))
  where scoreFrame ((10,_):rest@(a:b:_)) = (10+a+b,rest)
        scoreFrame (x:y:rest) | x+y < 10 = (x+y,rest)
                              | otherwise = (10+head rest,rest)

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ putStrLn $ lines doc

