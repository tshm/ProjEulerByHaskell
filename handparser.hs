import Control.Applicative
import System.Environment

-- | test
-- >>> parsplit "+" "(12+34)+22"
-- ("aa","")
parsplit :: [Char] -> String -> (String, String)
parsplit ops str = (l, r) where
  (n, l, r) = foldl trans (0, "", "") str
  trans (n, l, r) x = (n', l', r') where
    n' = case x of
      '(' -> n + 1
      ')' -> n - 1
      _   -> n
    l' = if not isHit && r == [] then l ++ [x] else l
    r' = if (r /= [] || elem x ops) then r ++ [x] else []
    isHit = elem x ops && n == 0

-- Simple Calculator
data Expr = Value Float
         | Add Expr Expr
         | Sub Expr Expr
         | Div Expr Expr
         | Mult Expr Expr
         | Exp Expr Expr
         deriving (Show)

-- | parse expression
-- >>> parseExpr "250+14.3"
-- Add (Value 250.0) (Value 14.3)
--
-- >>> parseExpr "10/250*14.3"
-- Div (Value 10.0) (Mult (Value 250.0) (Value 14.3))
--
-- >>> parseExpr "250*14.3"
-- Mult (Value 250.0) (Value 14.3)
--
-- >>> parseExpr "250*14.3+3"
-- Add (Mult (Value 250.0) (Value 14.3)) (Value 3.0)
--
-- >>> parseExpr "(3+250)*14.3"
-- Mult (Add (Value 3.0) (Value 250.0)) (Value 14.3)
--
-- >>> parseExpr "3+250*14.3"
-- Add (Value 3.0) (Mult (Value 250.0) (Value 14.3))
--
-- >>> parseExpr "3^2*14.3"
-- Mult (Exp (Value 3.0) (Value 2.0)) (Value 14.3)
--
-- >>> parseExpr "-3"
-- Value (-3.0)
parseExpr :: String -> Expr
parseExpr str = case splitResult of
    Just (left,'*':right) -> Mult (parseExpr left) (parseExpr right)
    Just (left,'/':right) -> Div (parseExpr left) (parseExpr right)
    Just (left,'+':right) -> Add (parseExpr left) (parseExpr right)
    Just (left,'-':right) -> Sub (parseExpr left) (parseExpr right)
    Just (left,'^':right) -> Exp (parseExpr left) (parseExpr right)
    Nothing               -> Value (read str :: Float)
  where
    splitResult = split "+-"
              <|> split "*/"
              <|> split "^"
    split operators = if rem == [] || isUnaryMinus
                      then Nothing
                      else Just (left, rem) where
      (left, rem) = break (\c -> elem c operators) str
      isUnaryMinus = (rem /= [] && '-' == head rem) && (left == [] || elem (last left) "(+-*/^")
-- 1 ()
-- 2 -

-- | evaluate tree
-- >>> evaluate $ Mult (Value 250.0) (Value 14.3)
-- 3575.0
evaluate :: Expr -> Float
evaluate exp = case exp of
  Mult a b -> (evaluate a) * (evaluate b)
  Div a b -> (evaluate a) / (evaluate b)
  Add a b -> (evaluate a) + (evaluate b)
  Sub a b -> (evaluate a) - (evaluate b)
  Value a -> a

-- | Trim zeros
-- >>> trim 3.0
-- "3"
--
-- >>> trim 3.001
-- "3"
trim :: Float -> String
trim x = left ++ right'' where
  (left, right) = break (=='.') $ show x
  right' = if length right > 3 then take 3 right else right
  right'' = if right' == ".0" || right' == ".00" then "" else right'


main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  putStr $ doc

