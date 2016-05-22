import System.Environment (getArgs)
import Data.Function
import Data.List
import Text.Printf
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

type Track = [(Double, Int)]
type Angle = Int
type Velocity = Double
type Distance = Double
type Time = Double
data Car = Car { top :: Velocity,
                 tacc :: Time,
                 tdec :: Time }  deriving (Show)

-- |
-- >>> head $ fst $ parse "1.029 115 1.122 125 1.185 100 0.53 110 0.751 95 1.242 85 0.533 85 1.003 120 0.465 110 0.546 125 0.446 90 0.582 70 0.878 45 0.49 30 1.016 130 1.047 140 1.146 75 0.496 85 0.857 125 0.971 0\n1 266 8.1 1.4"
-- (1.029,115)
--
-- >>> head $ snd $ parse "1.029 115 1.122 125 1.185 100 0.53 110 0.751 95 1.242 85 0.533 85 1.003 120 0.465 110 0.546 125 0.446 90 0.582 70 0.878 45 0.49 30 1.016 130 1.047 140 1.146 75 0.496 85 0.857 125 0.971 0\n1 266 8.1 1.4"
-- Car {top = 266.0, tacc = 8.1, tdec = 1.4}
--
parse :: String -> (Track, [Car])
parse str = (spl $ words str0, cars) where
  (str0:strs) = lines str
  spl (x0:x1:xs) = (read x0 :: Distance, read x1 :: Angle) : spl xs
  spl _ = []
  cars = map (parsecar . words) strs
  parsecar [_,x1,x2,x3] = Car (read x1 :: Velocity) (read x2 :: Time) (read x3 :: Time)
  parsecar _ = Car 0 0 0

-- |
-- >>> run1 (Car 10.0 1.0 1.0) 10.0 10.0 0
-- (3600.0,10.0)
--
-- >>> run1 (Car 10.0 1.0 1.0) 10.0 1.0 0
-- (360.0,10.0)
--
run1 :: Car       -- spec
     -> Velocity  -- initial value
     -> Distance -> Angle -> (Time, Velocity)
run1 (Car vm ta td) v0 l ang1 = (t0 + tf + t1, v1) where
  t0 = ta * (vm - v0) / vm
  t1 = td * (vm - v1) / vm
  tf = lf / (vm / 3600.0)
  v1 = vm * (1.0 - fromIntegral ang1 / 180.0)
  lf = l
     - 0.5 * (vm + v0) / 3600.0 * t0
     - 0.5 * (vm + v1) / 3600.0 * t1
  
track :: Track
(track, _:_) = parse "1.029 115 1.122 125 1.185 100 0.53 110 0.751 95 1.242 85 0.533 85 1.003 120 0.465 110 0.546 125 0.446 90 0.582 70 0.878 45 0.49 30 1.016 130 1.047 140 1.146 75 0.496 85 0.857 125 0.971 0\n1 266 8.1 1.4"

-- |
-- >>> printf "%0.2f" $ run (Car 266 4.0 1.9) track
-- 241.05
--
run :: Car -> Track -> Time
run car = run' car 0 where
  run' _ _ [] = 0.0
  run' car' v0 (x:xs') = t + run' car' v1 xs' where
    (t, v1) = uncurry (run1 car' v0) x

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ putStrLn $ fmt $ parse doc where
    fmt (trk, cars) = map (\(i,t) -> printf "%d %0.2f" i t) laps' where
      laps = map (\c -> run c trk) cars
      laps' = sortBy (compare `Data.Function.on` snd) $ zip [(1::Int)..] laps

