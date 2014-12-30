{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import System.Environment (getArgs)
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

slangTbl :: [String]
slangTbl = [
  ", yeah!",
  ", this is crazy, I tell ya.",
  ", can U believe this?",
  ", eh?",
  ", aw yea.",
  ", yo.",
  "? No way!",
  ". Awesome!"]

testIn :: C.ByteString
testIn = C.unlines [
  "Lorem ipsum dolor sit amet. Mea et habeo doming praesent. Te inani utroque recteque has, sea ne fugit verterem!",
  "Usu ei scripta phaedrum, an sed salutatus definiebas? Qui ut recteque gloriatur reformidans. Qui solum aeque sapientem cu.",
  "Eu nam nusquam quaestio principes."]

testOut :: C.ByteString
testOut = C.unlines [
  "Lorem ipsum dolor sit amet. Mea et habeo doming praesent, yeah! Te inani utroque recteque has, sea ne fugit verterem!",
  "Usu ei scripta phaedrum, an sed salutatus definiebas, this is crazy, I tell ya. Qui ut recteque gloriatur reformidans. Qui solum aeque sapientem cu, can U believe this?",
  "Eu nam nusquam quaestio principes."]

-- |
-- >>> enSlang "test. 123? asdf! wefas."
-- "test. 123, yeah! asdf! wefas, this is crazy, I tell ya."
--
-- >>> enSlang testIn == testOut
-- True
--
enSlang :: String -> String
enSlang str = enSlang' str False 0 where
  enSlang' [] _ _ = []
  enSlang' (x:xs) f i
    | elem x ".!?"  = if f 
                      then (slng i) ++ enSlang' xs (not f) (i+1)
                      else x : enSlang' xs (not f) i
    | otherwise     = x : enSlang' xs f i
  slng i = slangTbl !! (i `mod` (length slangTbl))

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  putStr $ enSlang doc


