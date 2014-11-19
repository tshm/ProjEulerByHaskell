import System.Environment
import Text.ParserCombinators.ReadP
import Text.Printf

-- Simple Calculator
data Expr = Value Double
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mult Expr Expr
          | Exp Expr Expr
          | Um Expr
          deriving (Show)

-- | Double number parser
-- >>> parseToExpr "52"
-- Value 52.0
--
-- >>> parseToExpr "-0.3"
-- Value (-0.3)
--
expr :: ReadP Expr
expr = do
  sign <- option '+' (char '-')
  expr'' <- expr'
  return $ if sign == '+'
           then expr''
           else Um expr''

expr' :: ReadP Expr
expr' = term `chainl1` (plus +++ minus) where
  plus  = char '+' >> return Add
  minus = char '-' >> return Sub

term :: ReadP Expr
term = factor' `chainl1` (mult +++ divn) where
  mult = char '*' >> return Mult
  divn = char '/' >> return Div

factor' :: ReadP Expr
factor' = factor `chainl1` exp' where
  exp' = char '^' >> return Exp

factor :: ReadP Expr
factor = number +++ paren where
  paren = between (char '(') (char ')') expr

number :: ReadP Expr
number = do
  sign <- option "" (string "-")
  str <- munch1 (`elem` "1234567890.")
  return $ Value (read (sign ++ str) :: Double)

{--
um :: ReadP Expr
um = do
  _ <- char '-'
  num <- number
  return $ Um num
--}

parseToExpr :: String -> Expr
parseToExpr = fst . last . readP_to_S expr

-- | mult and division
-- >>> parseToExpr "250*14.3"
-- Mult (Value 250.0) (Value 14.3)
--
-- >>> parseToExpr "10/250*14.3"
-- Mult (Div (Value 10.0) (Value 250.0)) (Value 14.3)
--
-- >>> parseToExpr "250+14.3*3"
-- Add (Value 250.0) (Mult (Value 14.3) (Value 3.0))
--
-- >>> parseToExpr "(3+250)*14.3"
-- Mult (Add (Value 3.0) (Value 250.0)) (Value 14.3)
--
-- >>> parseToExpr "3+250*14.3"
-- Add (Value 3.0) (Mult (Value 250.0) (Value 14.3))
--
-- >>> parseToExpr "3^2*14.3"
-- Mult (Exp (Value 3.0) (Value 2.0)) (Value 14.3)
--
-- >>> parseToExpr "3^(2+3)*14.3"
-- Mult (Exp (Value 3.0) (Add (Value 2.0) (Value 3.0))) (Value 14.3)
--
-- >>> parseToExpr "-(2+3)"
-- Um (Add (Value 2.0) (Value 3.0))
--

-- | evaluate tree
-- >>> evaluate $ Mult (Value 250.0) (Value 14.3)
-- 3575.0
--
-- >>> evaluate $ parseToExpr "600^501/600^500"
--
evaluate :: Expr -> Double
evaluate e = case e of
  Mult a b -> (evaluate a) * (evaluate b)
  Div a b -> (evaluate a) / (evaluate b)
  Add a b -> (evaluate a) + (evaluate b)
  Sub a b -> (evaluate a) - (evaluate b)
  Exp a b -> (evaluate a) ** (evaluate b)
  Um a    -> -(evaluate a)
  Value a -> a

-- | Trim zeros
-- >>> trimShow 3.0
-- "3"
--
-- >>> trimShow 3.001
-- "3.001"
--
-- >>> trimShow 0.1234567
-- "0.12346"
--
trimShow :: Double -> String
trimShow x = str where
  rounded = (fromIntegral (round (x * mag) :: Integer)) / mag
  str = reverse $ dropWhile (`elem` ".0") $ reverse $ printf "%f" rounded
  mag = 100000.0 :: Double

-- |
-- >>> delSpace "test test asd d"
-- "testtestasdd"
delSpace :: String -> String
delSpace = filter (/=' ')

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . trimShow . evaluate . parseToExpr . delSpace) (lines doc)

