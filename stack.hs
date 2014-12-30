import Control.Monad.State
-- newtype State s a = State { runState :: s -> (a, s) }

type Stack = [Int]

-- pop :: Stack -> (Int, Stack)
-- pop (x:xs) = (x, xs)
-- pop [] = error "zero size"
--
-- push :: Int -> Stack -> ((), Stack)
-- push x xs = ((), x:xs)

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

-- |
-- >>> put [1,2,3]
--
push :: Int -> State Stack ()
push x = state $ \xs -> ((), x:xs)

arr :: State Stack Int
arr = do
  push 3
  push 5
  x <- pop
  return x

main :: IO ()
main = print $ runState arr [1,2]

