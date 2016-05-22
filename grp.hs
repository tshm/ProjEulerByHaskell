import System.Environment (getArgs)
import Data.List (intercalate)
import qualified Data.Map as M
--import Text.Printf

data Network = Network {
  fr :: [String],
  grp :: [String]
} deriving (Show)

-- | get ratio of lowercase and uppercase letters
-- >>> parse "A:B,C,D:a,b,c"
-- [("A",Network {fr = ["B","C","D"], grp = ["a","b","c"]})]
--
-- >>> parse "nm:B,C,D:a,b b,c"
-- [("nm",Network {fr = ["B","C","D"], grp = ["a","b b","c"]})]
--
-- >>> parse "nm:B,C,D:"
-- [("nm",Network {fr = ["B","C","D"], grp = []})]
--
parse :: String -> [(String, Network)]
parse doc = map parseLine $ lines doc where
  parseLine str = (nm, Network { fr = f, grp = gr }) where
    splitBy c str' = filter (not . null) $ lst : ls where
      (ls,lst) = foldr (\c' (s,st) -> if c==c' then (st:s,[]) else (s,c':st)) ([],[]) str'
    (nm:frs:grs) = splitBy ':' str
    f = splitBy ',' frs
    gr = if not (null grs) then splitBy ',' (head grs) else []

-- | histogram
-- >>> histogram ["A","B","A","C"]
-- fromList [("A",2),("B",1),("C",1)]
--
histogram :: (Ord a) => [a] -> M.Map a Int
histogram = M.fromListWith (+) . (`zip` repeat 1)

-- | group suggestion
-- >>> group "A:B,C,D:a,b,c\nB:A,E:b,e\nC:A:e"
-- fromList [("A",["e"]),("B",[]),("C",["a","b","c"])]
--
-- >>> group "A:B,C,D:a,b,c,e\nB:A,E:b,e\nC:A:e"
-- fromList [("A",[]),("B",[]),("C",["a","b","c"])]
--
group :: String -> M.Map String [String]
group str = M.filter (not . null) $ M.mapWithKey getGrps nets where
  nets = M.fromList $ parse str
  getGrps nm net = filterGrp $ concatMap collectGrp frs where
    frs = fr net
    th = length frs
    orgGrp = grp $ M.findWithDefault Network{fr=[],grp=[]} nm nets
    collectGrp nm' = grp $ M.findWithDefault Network{fr=[],grp=[]} nm' nets
    filterGrp xs = M.keys $ M.filterWithKey (\k v -> v*2 >= th && notElem k orgGrp) $ histogram xs

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ putStrLn $ format $ group doc where
    format = map (\(k,xs) -> k ++ ":" ++ intercalate "," xs) . M.toAscList

