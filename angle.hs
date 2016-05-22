import System.Environment (getArgs)
import Text.Printf (printf)

-- |
-- >>> angle 330.39991833
-- "330.23'59\""
--
-- >>> angle 0.001
-- "0.00'03\""
--
-- >>> angle 14.64530319
-- "14.38'43\""
--
-- >>> angle 0.25
-- "0.15'00\""
-- 
-- >>> angle 254.16991217
-- "254.10'11\""
--
angle :: Double -> String
angle deg = printf "%d.%02d'%02d\"" deg' min sec where
  deg' = floor deg :: Int
  min0 = 60.0 * (deg - fromIntegral deg') 
  min  = floor min0 :: Int
  sec  = floor $ 60.0 * (min0 - fromIntegral min) :: Int

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . angle . (read :: String -> Double)) $ lines doc

