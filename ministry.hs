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

testIn :: String
testIn = unlines [
  "Lorem ipsum dolor sit amet. Mea et habeo doming praesent. Te inani utroque recteque has, sea ne fugit verterem!",
  "Usu ei scripta phaedrum, an sed salutatus definiebas? Qui ut recteque gloriatur reformidans. Qui solum aeque sapientem cu.",
  "Eu nam nusquam quaestio principes."]

testOut :: String
testOut = unlines [
  "Lorem ipsrum dolor sit amet. Mea et habeo doming praesent, yeah! Te inani utroque recteque has, sea ne fugit verterem!",
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
enSlang str = str' where
  ss = lines $ concatMap (\c -> if elem c ".!?" then c ++ "\n" else [c]) str 
  ss' = map (\(i,s) -> if even i then repl s else s) $ zip [1..] ss
  repl s = s ++ "."
  str' = unlines ss'

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  (putStr . enSlang) doc

