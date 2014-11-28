import System.Environment (getArgs)
import Data.List

data Color = RGB (Int, Int, Int)
           | HEX String
           | HSV (Int, Int, Int)
           | HSL (Int, Int, Int)
           | CMYK (Float, Float, Float, Float)
           deriving (Show, Read)

-- |
-- >>> color "(0.56,0.94,0.21,0.02)"
-- "RGB(110,15,197)"
--
-- >>> color "HSL(359,0,0)"
-- "RGB(0,0,0)"
--
-- >>> color "HSV(276,33,7)"
-- "RGB(15,12,18)"
--
-- >>> color "#cfa9c4"
-- "RGB(207,169,196)"
--
color :: String -> String
color str = str' where
  c = case str of
    '(':_  -> read ("CMYK " ++ str) :: Color
    'H':_  -> read str :: Color
    '#':_  -> read ("HEX \"" ++ str ++ "\"") :: Color
    _      -> error "invalid format."
  str' = "RGB" ++ ((words $ show $ toRGB c) !! 1)
  

-- |
-- >>> toRGB $ CMYK (0.56,0.94,0.21,0.02)
-- RGB (110,15,197)
--
-- >>> toRGB $ HSV (0,0,50)
-- RGB (128,128,128)
--
-- >>> toRGB $ HSV (0,100,50)
-- RGB (128,0,0)
--
-- >>> toRGB $ HSV (276,33,7)
-- RGB (15,12,18)
--
-- >>> toRGB $ HSL (359,0,0)
-- RGB (0,0,0)
--
-- >>> toRGB $ HEX "#cfa9c4"
-- RGB (207,169,196)
--
toRGB :: Color -> Color
toRGB (CMYK (c, m, y, k)) = RGB (r, g, b) where
  r = round $ f * (1 - c)
  g = round $ f * (1 - m)
  b = round $ f * (1 - y)
  f = 255 * (1 - k)
toRGB (HSV (h, s, v)) = RGB (round r, round g, round b) where
  s' = fromIntegral s * (0.01 :: Float)
  v' = fromIntegral v * (0.01 :: Float)
  c = v' * s'
  h' = (fromIntegral h) / 60.0
  h'' = h' - 2 * fromIntegral ((floor h') `div` 2::Integer)
  x = c * (1.0 - (abs (h'' - 1.0)))
  m = v' - c
  (r', g', b')
    |   0 <= h && h <  60  = (c, x, 0)
    |  60 <= h && h < 120  = (x, c, 0)
    | 120 <= h && h < 180  = (0, c, x)
    | 180 <= h && h < 240  = (0, x, c)
    | 240 <= h && h < 300  = (x, 0, c)
    | 300 <= h && h < 360  = (c, 0, x)
  (r, g, b) = (255 * (r' + m), 255 * (g' + m), 255 * (b' + m))
toRGB (HSL (h, s, l)) = RGB (round r, round g, round b) where
  s' = fromIntegral s * (0.01 :: Float)
  l' = fromIntegral l * (0.01 :: Float)
  c = (1.0 - abs (2.0 * l' - 1.0)) * s'
  h' = (fromIntegral h) / 60.0
  h'' = h' - 2 * fromIntegral ((floor h') `div` 2::Integer)
  x = c * (1.0 - (abs (h'' - 1.0)))
  m = l' - c / 2.0
  (r', g', b')
    |   0 <= h && h <  60  = (c, x, 0)
    |  60 <= h && h < 120  = (x, c, 0)
    | 120 <= h && h < 180  = (0, c, x)
    | 180 <= h && h < 240  = (0, x, c)
    | 240 <= h && h < 300  = (x, 0, c)
    | 300 <= h && h < 360  = (c, 0, x)
  (r, g, b) = (255 * (r' + m), 255 * (g' + m), 255 * (b' + m))
toRGB (HEX str) = RGB (r, g, b) where
  hexmap c = if c `elem` ['0'..'9']
             then Just (read [c] :: Int)
             else fmap (+10) $ elemIndex c ['a'..'f']
  parse h h' = d * 16 + d' where
    Just d  = hexmap h
    Just d' = hexmap h'
  r = parse (str !! 1) (str !! 2)
  g = parse (str !! 3) (str !! 4)
  b = parse (str !! 5) (str !! 6)
toRGB x = x

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . color) $ lines doc

