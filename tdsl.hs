import Data.List
import System.Environment

data Ope = SetCol Int Int
         | SetRow Int Int
         | QueryCol Int
         | QueryRow Int deriving (Show,Read)

opes :: [Ope]
opes = reverse [SetCol 32 20,
                SetRow 15 7,
                QueryRow 3,
                SetRow 16 31,
                QueryCol 32]

-- |
-- >>> parse opes
-- [5118,20]
--
parse :: [Ope] -> [Int]
parse = parse' []

parse' :: [Int] -> [Ope] -> [Int]
parse' ans (QueryCol n:os) = sum [ get i n os | i <- [0..255]] : parse' ans os
parse' ans (QueryRow n:os) = sum [ get n i os | i <- [0..255]] : parse' ans os
parse' ans [] = ans
parse' ans (_:os) = parse' ans os

get :: Int -> Int -> [Ope] -> Int
get k j (SetRow l v:os') = if k == l then v else get k j os'
get k j (SetCol l v:os') = if j == l then v else get k j os'
get k j (_:os') = get k j os'
get _ _ _ = 0


main :: IO ()
main = do
  [filename] <- getArgs
  strs <- (reverse . lines) `fmap` readFile filename
  mapM_ print $ reverse . parse $ map (\str -> read str :: Ope) strs


