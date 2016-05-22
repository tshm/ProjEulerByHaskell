import System.Environment (getArgs)
import Data.Char (toLower)

data Model = Model 
  { vampires :: Int
  , zombies :: Int
  , witches :: Int
  , houses :: Int
  } deriving (Show, Read)

-- |
-- >>> parse "Vampires: 1, Zombies: 1, Witches: 1, Houses: 1"
-- Model {vampires = 1, zombies = 1, witches = 1, houses = 1}
--
parse :: String -> Model
parse str = read str' :: Model where
  str' = "Model {" ++ map adapt str ++ "}"
  adapt c = if c == ':' then '=' else toLower c

-- |
-- >>> countCandy $ Model 1 1 1 1
-- 4
--
-- >>> countCandy $ Model 3 2 1 10
-- 36
--
countCandy :: Model -> Int
countCandy (Model v z w h) = div tot nc where
  nc = v + z + w
  tot = (3 * v + 4 * z + 5 * w) * h

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . countCandy . parse) $ lines doc

