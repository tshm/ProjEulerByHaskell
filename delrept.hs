{-# LANGUAGE OverloadedStrings #-}
--import qualified Data.ByteString.Char8 as C
import System.Environment (getArgs)
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> delrept "But as he spake he drew the good sword from its scabbard, and smote a heathen knight, Jusssstin of thee Iron Valley."
-- "But as he spake he drew the god sword from its scabard, and smote a heathen knight, Justin of the Iron Valey."
--
-- >>> delrept "No matttter whom you choose, she deccccclared, I will abide by your decision."
-- "No mater whom you chose, she declared, I wil abide by your decision."
--
-- >>> delrept "Wwwhat is your will?"
-- "Wwhat is your wil?"
--
-- >>> delrept "At his magic speech the ground oppened and he began the path of descent."
-- "At his magic spech the ground opened and he began the path of descent."
--
-- >>> delrept "I should fly away and you would never see me again."
-- "I should fly away and you would never se me again."
--
delrept :: String -> String
delrept [] = []
delrept (x:xs) = delrept' x xs where
  delrept' x0 [] = [x0]
  delrept' x0 (x0':xs0)
    | x0 == x0' = delrept' x0 xs0
    | otherwise = x0 : delrept' x0' xs0

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . delrept) $ lines doc

