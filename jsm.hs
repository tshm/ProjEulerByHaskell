import System.Environment
import Text.ParserCombinators.ReadP

-- JSON
data Object = NumValue Double
            | StrValue String
            | Object [(String, Object)]
            | Arr [Object]
            | Null
            deriving (Show)

-- | Double number parser
-- >>> parseJson "52"
-- NumValue 52.0
--
-- >>> parseJson "\"test\""
-- StrValue "test"
--
-- >>> parseJson "{\"key\": 33}"
-- Object [("key",NumValue 33.0)]
--
-- >>> parseJson "{\"key\": 33, \"aa\":\"t\"}"
-- Object [("key",NumValue 33.0),("aa",StrValue "t")]
--
-- >>> parseJson "[33, \"t\"]"
-- Arr [NumValue 33.0,StrValue "t"]
--
-- >>> parseJson "null"
-- Null
--
-- >>> parseJson "[{\"key\": 33}]"
-- Arr [Object [("key",NumValue 33.0)]]
--
parseJson :: String -> Object
parseJson = fst . last . readP_to_S json

json :: ReadP Object
json = jsonNum
     +++ jsonStr
     +++ jsonObj
     +++ jsonArr
     +++ jsonNull

jsonNull :: ReadP Object
jsonNull = do
  _ <- string "null"
  return Null

jsonArr :: ReadP Object
jsonArr = do
  _ <- char '['
  xs <- sepBy json $ skipMany (char ',' +++ char ' ')
  _ <- char ']'
  return $ Arr xs

jsonObj :: ReadP Object
jsonObj = do
  _ <- char '{'
  xs <- sepBy property $ skipMany (char ',' +++ char ' ')
  _ <- char '}'
  return $ Object xs

property :: ReadP (String, Object)
property = do
  key <- stringReader
  skipSpaces
  _ <- char ':'
  skipSpaces
  val <- json
  return (key, val)

jsonNum :: ReadP Object
jsonNum = do
  str <- munch1 (`elem` "1234567890.+-")
  return $ NumValue (read str :: Double)
  
jsonStr :: ReadP Object
jsonStr = do
  str <- stringReader
  return $ StrValue str

stringReader :: ReadP String
stringReader = between dq dq $ munch (/= '"')
  where dq = char '"'

-- |
-- >>> sumItemIds $ parseJson "{\"menu\": {\"header\": \"menu\", \"items\": [{\"id\": 27}, {\"id\": 0, \"label\": \"Label 0\"}, null, {\"id\": 93}, {\"id\": 85}, {\"id\": 54}, null, {\"id\": 46, \"label\": \"Label 46\"}]}}"
-- 46
--
sumItemIds :: Object -> Int
sumItemIds (Object xs) = case lookup "items" xs of
  Just arr -> sumItemIds' arr
  Nothing  -> sum $ map (sumItemIds . snd) xs
sumItemIds _ = 0

sumItemIds' :: Object -> Int
sumItemIds' (Arr objs) = sum $ map extractid objs where
  extractid (Object obj) = case lookup "label" obj of
    Just _ -> case lookup "id" obj of
      Just (NumValue v) -> round v
      Just _ -> 0
      Nothing -> 0
    Nothing -> 0 
  extractid _ = 0
sumItemIds' _ = 0

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (print . sumItemIds . parseJson) strs

