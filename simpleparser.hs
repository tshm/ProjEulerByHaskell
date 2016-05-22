import Control.Applicative
import System.Environment
import Text.Printf
import Debug.Trace

-- | round the number at the precision at 1e-5
-- >>> roundAt5 1
-- "1"
--
-- >>> roundAt5 0.0012345
-- "0.00123"
roundAt5 :: Float -> String
roundAt5 x = truncated where
  orig = printf "%.5f" x
  truncated = reverse . dropWhile flip elem ".0" . reverse $ orig

-- | split the expression by given operator list
-- >>> parsplit "+" "12+34"
-- ("12","+34")
--
-- >>> parsplit "+" "12-34+22"
-- ("12-34","+22")
--
-- >>> parsplit "+" "(12+34)+22"
-- ("12+34","+22")
--
-- >>> parsplit "-" "-(12+34)"
-- ("","-(12+34)")
parsplit :: String -> String -> (String, String)
parsplit ops str = (removeParen l, removeParen r) where
  (l, r) = if idx < 0 then (str, "") else splitAt idx str
  (_, idx, _) = foldr trans (0, -1, length str) str
  trans c (level, idx, cnt) = (level', idx', cnt') where
    level' = case c of
      ')' -> level + 1
      '(' -> level - 1
      _ -> level
    idx' = if hit then cnt - 1 else idx
    hit = level == 0 && idx < 0 && elem c ops
    cnt' = cnt - 1

-- | remove paren chars
-- >>> removeParen "(23+22)"
-- "23+22"
removeParen :: String -> String
removeParen str =
  if str /= [] && head str == '(' && last str == ')'
  then init . tail $ str
  else str

-- expression type
data Expr = Value Float
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mult Expr Expr
          | Exp Expr Expr
          | Min Expr
          deriving (Show)

-- | parse string into expression
-- >>> parseExpr "250+14.3"
-- Add (Value 250.0) (Value 14.3)
--
-- >>> parseExpr "10/250*14.3"
-- Mult (Div (Value 10.0) (Value 250.0)) (Value 14.3)
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
-- Min (Value 3.0)
--
-- >>> parseExpr "-(253 / 5)"
-- Min (Div (Value 253.0) (Value 5.0))
--
-- >>> parseExpr "3^(-253)"
-- Exp (Value 3.0) (Min (Value 253.0))
--
-- >>> parseExpr "5 - 5 + 3"
-- Add (Sub (Value 5.0) (Value 5.0)) (Value 3.0)
--
parseExpr :: String -> Expr
parseExpr str = case splitResult of
  Just (left,'*':right) -> Mult (parseExpr left) (parseExpr right)
  Just (left,'/':right) -> Div (parseExpr left) (parseExpr right)
  Just (left,'+':right) -> Add (parseExpr left) (parseExpr right)
  Just (left,'-':right) -> Sub (parseExpr left) (parseExpr right)
  Just (left,'^':right) -> Exp (parseExpr left) (parseExpr right)
  Just ('-':expr,"") -> Min (parseExpr $ removeParen expr)
  Nothing -> Value (read $ removeParen str :: Float)
  where
    splitResult = split "+-"
              <|> split "*/"
              <|> split "^"
              <|> findMinus
    split operators = if isUnaryMinus || null rem
                      then Nothing
                      else Just (left, rem) where
      (left, rem) = parsplit operators $ filter (/=' ') str
      isUnaryMinus = (rem /= [] && '-' == head rem)
        && (null left || elem (last left) "(+-*/^")
    findMinus = if null rem
                then Nothing
                --else Just ((traceShow rem rem),"") where
                else Just (removeParen rem,"") where
      (left, rem) = parsplit "-" $ filter (/=' ') $ removeParen str

-- | evaluate tree
-- >>> evaluate $ Mult (Value 250.0) (Value 14.3)
-- 3575.0
evaluate :: Expr -> Float
evaluate exp = case exp of
  Exp a b -> evaluate a ** evaluate b
  Mult a b -> evaluate a * evaluate b
  Div a b -> evaluate a / evaluate b
  Add a b -> evaluate a + evaluate b
  Sub a b -> evaluate a - evaluate b
  Min a -> -(evaluate a)
  Value a -> a

-- | evaluate the simple expression and print
--   output of the formatted string.
-- >>> evaluateExpr "250*14.3"
-- "3575"
--
-- >>> evaluateExpr "3^6 / 117"
-- "6.23077"
--
-- >>> evaluateExpr "(2.16 - 48.34)^-1"
-- "-0.02165"
--
-- >>> evaluateExpr "(59 - 15 + 3*6)/21"
-- "2.95238"
--
evaluateExpr :: String -> String
evaluateExpr = roundAt5 . evaluate . parseExpr

main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . evaluateExpr) $ lines doc

