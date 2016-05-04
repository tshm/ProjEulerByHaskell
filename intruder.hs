import System.Environment (getArgs)
import Text.ParserCombinators.ReadP
import Control.Monad

-- |
-- >>> head $ readP_to_S parseDecs "11.22.33.44"
-- (IP (11,22,33,44),"")
--
-- >>> head $ readP_to_S parseDecs "xx11.22.33.44yy"
-- (IP (11,22,33,44),"yy")
--
data IP = IP (Int, Int, Int, Int) deriving (Show)

parseDecs :: ReadP IP
parseDecs = do
  skipMany $ satisfy (\_ -> True)
  x0 <- pp
  _ <- char '.'
  x1 <- pp
  _ <- char '.'
  x2 <- pp
  _ <- char '.'
  x3 <- pp
  guard $ x0 > 0 && x0 < 256 && x1 < 256 && x2 < 256 && x3 < 256
  return $ IP (x0, x1, x2, x3)
    where
      pp = fmap (read :: String -> Int) $ munch (`elem` ['0'..'9'])

-- |
-- >>> head $ readP_to_S parseDec "3221226219"
--
parseDec :: ReadP IP
parseDec = do
  skipMany $ satisfy (\_ -> True)
  x <- fmap (read :: String -> Integer) $ munch (`elem` ['0'..'9'])
  return $ breakDown x where
    breakDown d = IP (fromInteger x0, fromInteger x1, fromInteger x2, fromInteger x3) where
      (xx0, x0) = d `divMod` 256
      (xx1, x1) = xx0 `divMod` 256
      (xx2, x2) = xx1 `divMod` 256
      (_, x3) = xx2 `divMod` 256

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn) $ lines doc

