import System.Environment

data Stack a = Bottom | Stack (Elem a) (Stack a)  deriving (Show)
data Elem a = Elem a  deriving (Show)

-- |
-- >>> push 1 Bottom
-- Stack (Elem 1) Bottom
--
-- >>> push 5 (Stack (Elem 3) Bottom)
-- Stack (Elem 5) (Stack (Elem 3) Bottom)
--
push :: a -> Stack a -> Stack a
push x stk = Stack (Elem x) stk

-- |
-- >>> pop (Stack (Elem 3) Bottom)
-- (3,Bottom)
--
-- >>> pop (Stack (Elem 5) (Stack (Elem 3) Bottom))
-- (5,Stack (Elem 3) Bottom)
--
pop :: Stack a -> (a, Stack a)
pop Bottom = error "empty stack"
pop (Stack (Elem h) t) = (h, t)

-- |
-- >>> getAlt [1,2,3,4]
-- [4,2]
--
-- >>> getAlt [3,5,22,1,66,22,33]
-- [33,66,22,3]
--
getAlt :: [a] -> [a]
getAlt xs = popAlt True [] stk where
  stk = foldl (\s x -> push x s) Bottom xs
  popAlt _ o Bottom = o
  popAlt True o s = popAlt False (o ++ [e]) s' where (e,s') = pop s
  popAlt False o s = popAlt True o s' where (_,s') = pop s
               
main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (putStrLn . format . getAlt . parse) strs where
    parse = words
    format = unwords


